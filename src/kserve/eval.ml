let parse_gensl_to_canonical raw_gensl =
  let open Genslib in
  let open Intf in
  raw_gensl
  |> Lexing.from_string
  |> Parsing.ParserTypes.pstate
  |> Parser.Default.read_top
  |> (function
      | Ok (toplevel, _) -> toplevel
      | _e -> failwith "parse error")
  |> Parsetreeflavor.to_canonicaltree
