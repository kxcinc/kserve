let sprintf fmt = Format.asprintf fmt
let eprintf fmt = Format.eprintf fmt
let pp_sexp = Sexplib.Sexp.pp_hum

module Globals0 = struct
  let program_starts = Unix.gettimeofday()
  let color_enabled = ArgOptions.has_flag "-no-color" |> not
  let upstream_conn_timeout = 1. (* sec *)
end

let colored ?style ?color_mode:(m=`Fg) color ppf str =
  if Globals0.color_enabled then (
    let code_table = function
      (* `Color -> (fg_code, bg_code) *)
      | `Black -> 30, 40
      | `Red -> 31, 41
      | `Green -> 32, 42
      | `Yellow -> 33, 43
      | `Blue -> 34, 44
      | `Magenta -> 35, 45
      | `Cyan -> 36, 46
      | `White -> 37, 47

      | `Bright_black -> 90, 100
      | `Bright_red -> 91, 101
      | `Bright_green -> 92, 102
      | `Bright_yellow -> 93, 103
      | `Bright_blue -> 94, 104
      | `Bright_magenta -> 95, 105
      | `Bright_cyan -> 96, 106
    in
    let style_table = function
      | `Bold -> 1
      | `Thin -> 2
      | `Italic -> 3
      | `Underline -> 4
    in
    let esc x = "\027"^x in
    let reset = "[0m" in
    let color_code =
      code_table color 
      |> (match m with `Fg -> fst | `Bg -> snd)
      |> sprintf "[%dm" in
    let style_code = style |> function
      | None -> None
      | Some s -> style_table s |> sprintf "[%dm" |> Option.some in
    Format.fprintf ppf
      "@<0>%s%s@<0>%s"
      ((esc color_code)
       ^(style_code |> Option.map esc |> Option.v "")) str (esc reset)
  ) else pp_string ppf str

module Logging = struct
  open Format
  let logging_formatter = err_formatter
  let log_raw fmt = fprintf logging_formatter fmt
  let log ~label
        ?header_style:(style=None)
        ?header_color:(color=`Magenta)
        fmt =
    let t = Unix.gettimeofday() -. Globals0.program_starts in
    let header = sprintf "[%s :%.3f]" label t in
    let pp_header = colored ?style color in
    log_raw "%a @[<hov 2>" pp_header header;
    kfprintf (fun ppf -> fprintf  ppf "@]@.")
      logging_formatter fmt
  let access fmt = log fmt ~label:"ACCESS" ~header_style:None ~header_color:`Cyan
  let info fmt = log fmt ~label:"INFO" ~header_style:(Some `Thin) ~header_color:`Bright_cyan
  let warn fmt = log fmt ~label:"WARN" ~header_style:(Some `Bold) ~header_color:`Yellow
  let debug fmt = log fmt ~label:"DEBUG" ~header_style:(Some `Bold) ~header_color:`Magenta
  let error fmt = log fmt ~label:"ERROR" ~header_style:(Some `Bold) ~header_color:`Red
end open Logging

module PrivateUtils = struct
  let categorize_path path =
    match Sys.(file_exists path, file_exists path && is_directory path) with
    | false, _ -> `Non_exsists
    | true, false -> `File
    | true, true -> `Directory
  let may_replace_assoc (k, v) l =
    if List.mem_assoc k l then l
    else (k, v) :: (List.remove_assoc k l)
end open PrivateUtils

open Lwt.Infix
open Httpaf
open Httpaf_lwt_unix

module Content_digest = struct
  include Digestif.MD5
  let hex_hash_of_string str =
    feed_string empty str |> get |> to_hex
end

(* XXX lwt-ify *)
module Assets : sig
  type asset
  type asset_entry = {
    mutable contents : Bigstringaf.t;
    mutable len : int;
    mutable stats : Unix.stats;
    mutable digest : Content_digest.t;
    mutable digest_hex : string;
    name : string;
    source : string;
    fd : Unix.file_descr;
    media_type : string;
  }

  val asset_of_path :
    media_type:string ->
    string -> asset

  val load : asset -> asset_entry

  val need_reload : asset -> bool
  val fresh_mtime : asset -> float
end = struct

  type asset_entry = {
      mutable contents : Bigstringaf.t;
      mutable len   : int;
      mutable stats : Unix.stats;
      mutable digest : Content_digest.t;
      mutable digest_hex : string;
      name : string;
      source : string;
      fd : Unix.file_descr;
      media_type : string;
    }

  type asset = asset_entry lazy_t

  let do_load fd stats =
    let len = stats.Unix.st_size in
    let contents : Bigstringaf.t = Bigarray.(
        Unix.map_file fd Char C_layout false [|len|]
        |> array1_of_genarray) in
    let digest = Content_digest.(
        feed_bigstring empty contents |> get) in
    let digest_hex = Content_digest.to_hex digest in
    len, contents, digest, digest_hex

  let asset_of_path ~media_type path : asset = lazy (
    let fd = Unix.openfile path [O_RDONLY] 0o640 in
    let stats = Unix.fstat fd in
    let len, contents, digest, digest_hex =
      do_load fd stats in
    let name = path in
    let source =
      let open Filename in
      if is_relative path
      then (sprintf "file://./%s" path)
      else (sprintf "file://%s" path) in
    info "asset %s loaded, digest = %s"
      name digest_hex;
    at_exit (fun () -> Unix.close fd);
    { contents; len; stats; media_type;
      digest; digest_hex; fd; name; source })

  let need_reload asset =
    let entry = Lazy.force asset in
    let stats' = Unix.fstat entry.fd in
    entry.stats.st_mtime < stats'.st_mtime

  let fresh_mtime asset =
    let fd = (Lazy.force asset).fd in
    Unix.(fstat fd).st_mtime

  let load asset =
    let entry = Lazy.force asset in
    let stats' = Unix.fstat entry.fd in
    if entry.stats.st_mtime < stats'.Unix.st_mtime then (
      let old_digest = entry.digest_hex in
      info "reloading asset %s with digest %s" entry.name old_digest;
      let len, contents, digest, digest_hex =
        do_load entry.fd stats' in
      info "asset %s reloaded, digest : %s => %s"
        entry.name old_digest digest_hex;
      entry.len <- len;
      entry.contents <- contents;
      entry.stats <- stats';
      entry.digest <- digest;
      entry.digest_hex <- digest_hex;
    ); entry

end

let guess_media_type path =
  let basename = Filename.(path |> basename |> String.lowercase_ascii) in
  let ext = basename |> Filename.extension in
  match basename, ext with
  | _, ".html" | _, ".htm" -> "text/html"
  | _, ".txt" | _, ".md" -> "text/plain"
  | _, ".css" -> "text/css"
  | _, ".js" -> "text/javascript"

  | "dune", _ | "dune-project", _ | "ml", _
    -> "text/plain"

  | _ ->
     warn "unknown media type for file: %s (ext=%s)" basename ext;
     "application/octet-stream"

module Globals = struct
  include Globals0

  let asset_root =
    match ArgOptions.(get_option (StringOption "-root")) with
    | None ->
       error "you must specify the -root argument as the asset root";
       exit 2
    | Some p ->
       if not Sys.(file_exists p && is_directory p) then (
         error "asset root specified by the -root argument (%s)@;\
                does note exist or is not a directory" p;
         exit 2
       ) else p

  let present_working_directory = Sys.getcwd()

  let welcome_file, welcome_file_path =
    let open MonadOps(Option) in
    (ArgOptions.(get_option (StringOption "-welcome")) >>= fun path ->
     Sys.(file_exists path && (not (is_directory path))) |> Option.of_bool >>= fun () ->
     let media_type = guess_media_type path in
     (Assets.asset_of_path ~media_type path, path) |> pure)
    |> function
      | None -> None, None
      | Some (asset, path) -> Some asset, Some path

  let spa =
    let spa = ArgOptions.has_flag "-spa" in
    if spa && (Option.is_none welcome_file) then (
      error "-spa : you must specify a welcome file";
      exit 2
    ) else spa

  let extensions_to_hide =
    if ArgOptions.has_flag "-hide-htmlext"
    then [".html"; ".htm"]
    else []

  let upstream_mappings :
        target:string ->
        (string (* upstream *) * string (* url *)) option =
    match ArgOptions.(get_option (StringOption "-upstream")) with
    | None -> fun ~target:_ -> None
    | Some maps -> (
      let malformed_argument() =
        error "malformed -upstream: must be of form '<target pattern> => <upstream pattern>'@\n\
               e.g. '/api/... => http://localhost:4321/...'";
        exit 2 in
      let parse_single map =
        match Str.split_delim (Str.regexp_string " => ") map with
        | [target_patt; upstream_patt] ->
           let split = Str.(split_delim (regexp_string "...")) in
           (match split target_patt, split upstream_patt with
            | [target_before; target_after], [path_before; path_after] ->
               let target_regexp =
                 let open Str in
                 sprintf "%s\\(.*\\)%s" (quote target_before) (quote target_after)
                 |> regexp in
               fun ~target ->
               let open Str in
               if string_match target_regexp target 0 then (
                 let subtarget = matched_group 1 target in
                 Some (upstream_patt, path_before^subtarget^path_after)
               ) else None
            | _ -> malformed_argument())
        | _ -> malformed_argument() in
      let mappings =
        String.split_on_char ';' maps
        |&> String.trim
        |&> parse_single in
      fun ~target ->
      let rec loop = function
        | [] -> None
        | head :: rest ->
           (match head ~target with
            | Some r -> Some r
            | None -> loop rest) in
      loop mappings)
end

let additional_headers ?target:_ headers =
  headers
  |> may_replace_assoc ("Server", Versions.software_version_desc)

let stream_asset_with_prefix
      ?prefix
      ?headers:(headers=[])
      ?status:(status=`OK)
      ?cache_control
      ~contents reqd =
  let { Assets.contents; len = clen;
        media_type; digest_hex; _ } = contents in
  let prefix, etag = match prefix with
    | None -> "", digest_hex
    | Some str ->
       str, Content_digest.(
         sprintf "%s-%s" digest_hex
           (hex_hash_of_string str)) in
  let open MonadOps(Option) in
  let { Request.meth; target; headers = reqheaders; _ } =
    Reqd.request reqd in
  match Headers.get reqheaders "If-None-Match" with
  | Some client_etag when (client_etag = etag) ->
     let headers = Headers.of_list ([
                       "Content-Length", "0";
                       "Etag", etag] |> additional_headers ~target) in
     let resp = Response.create ~headers `Not_modified in
     Reqd.respond_with_string reqd resp ""
  | _ -> begin
      let len = clen + String.length prefix in
      let headers =
        let may_replace (header, value) headers =
          if List.mem_assoc header headers then headers
          else (header, value) :: headers in
        headers
        |> may_replace ("Content-Length", len |> string_of_int)
        |> may_replace ("Content-Type", media_type)
        |> may_replace ("ETag", etag)
        |> additional_headers ~target
        |> Headers.of_list in
      let headers =
        let verb = match cache_control with
          | Some `No_store -> Some "no-store"
          | Some `No_cache -> Some "no-cache"
          | Some `Public -> Some "public"
          | Some `Private | None -> Some "private" in
        match verb with
        | None -> headers
        | Some verb ->
           Headers.add_unless_exists headers
             "Cache-Control" verb in
      let resp = Response.create ~headers status in
      let body =
        Reqd.respond_with_streaming reqd resp
          ~flush_headers_immediately:true in
      if meth <> `HEAD then (
        Body.write_string body prefix;
        Body.schedule_bigstring body contents);
      Body.flush body (fun () -> Body.close_writer body)
    end

let pp_sockaddr ppf =
  let open Unix in
  let print fmt = Format.fprintf ppf fmt in
  function
  | ADDR_UNIX path -> print "unix:%s" path
  | ADDR_INET (inet, port) -> print "%s:%d" (string_of_inet_addr inet) port

module AssetCache = struct
  (* XXX - change to LRU or alike *)
  module Private = struct
    let cache = Hashtbl.create 100
  end open Private

  let get ?media_type path =
    let media_type = match media_type with
      | Some typ -> typ
      | None -> guess_media_type path in
    match Hashtbl.find_opt cache path with
    | Some asset -> asset
    | None ->
       let asset = Assets.asset_of_path ~media_type path in
       Hashtbl.add cache path asset;
       asset
end

let respond_reqd_with_string
      ?headers:(headers=[])
      ?status:(status=`OK)
      reqd str =
  let headers = ("Content-Length", String.length str |> string_of_int) :: headers in
  let headers = headers |> additional_headers ~target:(Reqd.request reqd).target
                |> Headers.of_list in
  let resp = Response.create ~headers status in
  let body =
    Reqd.respond_with_streaming reqd resp
      ~flush_headers_immediately:true in
  if (Reqd.request reqd).meth <> `HEAD then (
    Body.write_string body str);
  Body.flush body (fun () -> Body.close_writer body)

(* XXX - amend headers *)
let handle_upstream ?remote_ip ~upstream ~url reqd =
  let module Client = Cohttp_lwt_unix.Client in
  let url = url |> Uri.of_string in
  let req = Reqd.request reqd in
  let with_timeout ~timeout v progn =
    let timeout =
      Lwt_unix.sleep timeout >>= fun () ->
      Lwt.return v in
    Lwt.pick [progn; timeout] in
  let host = Headers.get_exn req.headers "host" in
  let headers =
    let remote_ip = match remote_ip with
      | None -> identity
      | Some ip -> may_replace_assoc ("x-forwarded-for", ip) in
    req.headers
    |> Headers.to_list
    |&> (fun (k,v) -> String.lowercase_ascii k, v)
    |> List.remove_assoc "host"
    |> may_replace_assoc ("x-forwarded-host", host)
    |> remote_ip
    |> Cohttp.Header.of_list in
  let body, start_body_transfer =
    let reqbody = Reqd.request_body reqd in
    let s, wr = Lwt_stream.create() in
    let rec on_read bs ~off ~len =
      Bigstringaf.substring bs ~off ~len
      |> Option.some |> wr;
      Body.schedule_read reqbody ~on_read ~on_eof
    and on_eof() = wr None in
    `Stream s, (fun () ->
      Body.schedule_read reqbody ~on_read ~on_eof) in
  match req.meth with
  | #Method.standard as meth ->
     let progn() =
       let timeout = Globals.upstream_conn_timeout in
       with_timeout ~timeout None
         (Client.call ~body ~headers meth url
          |> Fn.tap (fun _ -> start_body_transfer())
          >|= Option.some) >>= function
       | None ->
          error "upstream connection timeout; upstream=%s; url=%s"
            upstream (Uri.to_string url);
          respond_reqd_with_string ~status:`Internal_server_error reqd
                   "timeout making connection to upstream";
          Lwt.return_unit
       | Some (resp, ubody) ->
         let stream = Cohttp_lwt.Body.to_stream ubody in
         let { Cohttp.Response.headers; status; _ } = resp in
         let headers = headers |> Cohttp.Header.to_list |> Headers.of_list in
         let status = Cohttp.Code.code_of_status status |> Status.of_code in
         let cbody =
           Response.create ~headers status
           |> Reqd.respond_with_streaming reqd in
         stream |> Lwt_stream.iter (fun chunk ->
                       Body.write_string cbody chunk) >>= fun () ->
         Lwt_stream.closed stream >>= fun () ->
         Body.flush cbody (fun () -> Body.close_writer cbody);
         Lwt.return_unit in
     Lwt.async progn
  | _ ->
     error "unsupported HTTP method in handle_upstream: %a"
       Method.pp_hum req.meth;
     respond_reqd_with_string ~status:`Not_implemented reqd
       (sprintf "http request method (%a) not supported"
          Method.pp_hum req.meth)

let respond_with_404 ?path reqd =
  let path = match path with
    | None -> (Reqd.request reqd).target
    | Some p -> p in
  access "Route not found: %s" path;
  let resp = sprintf "Error 404 : requested route not found (%s)" path in
  respond_reqd_with_string ~status:`Not_found reqd resp

exception Invalid_path of string

let handle_exception exn start_response =
  let msg =
    let open Printexc in
    let exn_str = to_string exn in
    warn "Exception caught: %s@\n%a"
      exn_str
      pp_multiline (get_backtrace());
    exn_str
  in
  let headers =
    Headers.of_list ([
          "Content-Length", String.length msg |> string_of_int
        ] |> additional_headers) in
  let body = start_response headers in
  Body.write_string body msg;
  Body.flush body (fun () -> Body.close_writer body)

let dir_dim_regexp = Str.regexp_string Filename.dir_sep

let request_handler addr reqd =
  let req = Reqd.request reqd in
  access "income request from %a:@;%a"
    pp_sockaddr addr
    Request.pp_hum req;
  try let target = req.target in
      match Globals.upstream_mappings ~target with
      | Some (upstream, url) ->
         let remote_ip = match addr with
           | Unix.ADDR_UNIX _ -> None
           | ADDR_INET (inet, _) -> Some (Unix.string_of_inet_addr inet)
         in
         access "relayed to upstream %s => %s" target url;
         handle_upstream ?remote_ip ~upstream ~url reqd
      | None -> (
        let asset_path =
          (* as a security measure, we'd like to check that
           target does not has components referring to parent folders *)
          if Str.split_delim dir_dim_regexp target
             |> List.mem Filename.parent_dir_name then (
            raise (Invalid_path target));
          (* also partly as as a security measure,
           we make sure that the path starts with the [asset_root] *)
          Globals.asset_root^target in
        let extended_asset_path ext = asset_path^ext in
        let unsupported_method = function
          | `TRACE | `CONNECT  | `DELETE | `OPTIONS | `Other _
            -> true
          | `GET | `HEAD | `POST | `PUT
            -> false in
        let serve_path path =
          let asset = AssetCache.get path in
          (* let media_type = guess_media_type path in
           * let asset = Assets.asset_of_path ~media_type path in *)
          let contents = Assets.load asset in
          stream_asset_with_prefix reqd ~contents in
        let try_serve_unhidden_extensions exts =
          (* return true if served *)
          let has_category cat path = categorize_path path = cat in
          let open MonadOps(Option) in
          (exts |&> extended_asset_path
           |> List.find_opt (has_category `File) >>= fun path ->
           info "serving %s for %s" path target;
           serve_path path; pure ()) |> Option.is_some in
        let have_welcome_file = Option.is_some Globals.welcome_file in
        let serve_welcome_file() =
           let contents = Globals.welcome_file |> Option.get |> Assets.load in
           stream_asset_with_prefix reqd ~contents in
        match
          target, req.meth,
          categorize_path asset_path with
        | _ when unsupported_method req.meth ->
           respond_reqd_with_string ~status:`Not_implemented reqd
             (sprintf "http request method (%a) not supported"
                Method.pp_hum req.meth)
        | "/", _, _ when have_welcome_file ->
           serve_welcome_file()
        | _, `GET, `File -> serve_path asset_path
        | _, `GET, `Directory ->
           respond_reqd_with_string ~status:`Service_unavailable reqd
             (sprintf "%s is a directory" target)
        | "/__kserve-dev/parse-gensl", `POST, _ ->
          let open Genslib in
          let open Intf in
          let read = ref [] in
           let reqbody = Reqd.request_body reqd in
           let rec on_read buffer ~off ~len =
             refappend (Bigstringaf.substring buffer ~off ~len) read;
             Body.schedule_read reqbody ~on_eof ~on_read
           and on_eof () =
             let raw_gensl_string = (String.concat "" (List.rev !read)) in
             let canonical_gensl =
               Eval.parse_gensl_to_canonical raw_gensl_string
             in
             let canonical_gensl_string =
               CanonicaltreeFlavor.to_string canonical_gensl
             in
             let headers =
               ["Content-Type", "text/x.genslx";
                "Content-Length", canonical_gensl_string
                                  |> String.length
                                  |> string_of_int;
                "Connection", "close"] in
             respond_reqd_with_string ~headers reqd canonical_gensl_string
           in
           Body.schedule_read reqbody ~on_eof ~on_read
        | _ ->
           if try_serve_unhidden_extensions Globals.extensions_to_hide then ()
           else if have_welcome_file && Globals.spa then (
             serve_welcome_file()
           ) else respond_with_404 reqd)
  with
  | Invalid_path path ->
     warn "received request with invalid path: %s" path;
     respond_reqd_with_string ~status:`Bad_request reqd
       (sprintf "malformed request path: %s" path)
  | exn ->
     let start_response headers =
       let resp = Response.create ~headers `Internal_server_error in
       Reqd.respond_with_streaming ~flush_headers_immediately:true reqd resp in
     handle_exception exn start_response

let error_handler _ ?request:_ error start_response =
  match error with
  | `Exn exn -> handle_exception exn start_response
  | #Status.standard as error ->
     let msg = Status.default_reason_phrase error in
     let headers =
       Headers.of_list ([
             "Content-Length", String.length msg |> string_of_int
           ] |> additional_headers) in
     let body = start_response headers in
     Body.write_string body msg;
     Body.flush body (fun () -> Body.close_writer body)

let main() =
  let open Kxclib.ArgOptions in
  let bind_addr, bind_desc, hostname = match get_option (StringOption "-bind") with
    | None -> Unix.inet_addr_loopback, "loopback", "localhost"
    | Some addr ->
       Unix.inet_addr_of_string addr, addr, addr in
  let port = get_option_d (IntOption "-port") 7000 in
  let listen_address = Unix.(ADDR_INET (bind_addr, port)) in
  let scheme, scheme_str =
    if has_flag "-ssl"
    (* then "https", "HTTP Secure (https)"
     * else "http", "HTTP Plain  (http )" *)
    then `Http_secure, "https"
    else `Http_plain, "http" in
  let https_config = match get_option (StringOption "-ssl") with
    | Some certkey ->
       (match String.split_on_char ':' certkey with
        | [certfile; keyfile] -> Some (certfile, keyfile)
        | _ ->
           error "option -ssl must has argument in form '<certfile-path>:<keyfile-path>'";
           exit 2)
    | None -> None in
  let mkserver () sock conn =
    match https_config with
    | Some (certfile, keyfile) ->
       Lwt.catch (fun () ->
           Server.TLS.create_connection_handler
             ~certfile ~keyfile
             ~request_handler
             ~error_handler
             sock conn)
         Printexc.(fun exn ->
        error "exception caught via Lwt.catch: %s@;%a"
          (to_string exn)
          pp_multiline (get_backtrace());
        Lwt.return_unit)
    | None ->
       Server.create_connection_handler
         ~request_handler
         ~error_handler
         sock conn
  in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket
        listen_address (mkserver())
      >|= fun _ ->
      let pp_timestamp ppf timestamp = Ptime.(
          of_float_s timestamp |> Option.get
          |> to_rfc3339 ~tz_offset_s:0
          |> pp_string ppf) in
      let pp_scheme ppf =
        function
        | `Http_secure ->
           (colored ~style:`Italic `Green)
             ppf "Http Secure (https)"
        | `Http_plain ->
           (colored ~style:`Italic `Blue)
             ppf "Http Plain (http)"
      in
      info "%s started as %a server, listening at %s on port %d"
        Versions.software_name
        pp_scheme scheme
        bind_desc port;
      info "      server software : %a"
        (colored ~style:`Bold `White) Versions.software_version_desc;
      info "      software binary : %s" Sys.executable_name;
      info "         listening at : %s"
        (sprintf "%s://%s:%d" scheme_str hostname port);
      info "    startup timestamp : %.3f (i.e. %a)"
        Globals.program_starts
        pp_timestamp Globals.program_starts;
      info "current workdir (cwd) : %s" Globals.present_working_directory;

      info "%a@\n@\n%a"
             (colored ~style:`Underline `Bright_magenta)
             "This Software Comes With Absolutely No Support Nor Warranty"
             pp_multiline
             "THE SOFTWARE IS PROVIDED \"AS IS\",\n\
              WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,\n\
              INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF\n\
              MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.\n\
              \n\
              IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS\n\
              BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,\n\
              WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,\n\
              ARISING FROM, OUT OF OR IN CONNECTION WITH\n\
              THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."
      ;

      let config fmt = log fmt ~label:"CONFIG" ~header_style:None ~header_color:`Cyan in
      let pp_rpath ppf path =
        let fprintf fmt = Format.fprintf ppf fmt in
        fprintf "%s" path;
        if Filename.is_relative path then (
          fprintf "@ (relative to cwd)"
        ) in
      config "asset root : %a" pp_rpath Globals.asset_root;
      Globals.welcome_file_path |> Option.iter (fun path ->
        config "welcome file : %a" pp_rpath path;
        if Globals.spa then (
          config "as a single-page-application serving : %a" pp_rpath path
        ));
      https_config |> Option.iter (fun (certpath, keypath) ->
        config "ssl certificate : %a" pp_rpath certpath;
        config "ssl private key : %a" pp_rpath keypath);
    );
  let forever, _ =
    Lwt.wait () in
  Lwt_main.run forever

let () = main()
