let () =
  let open Format in
  let open Genslib.Intf in
  let open Gensl_eval in
  let open Eval in
  let rec loop () =
    printf "repl> "; print_flush ();
    let line = read_line () in
    match line with
    | "#quit" -> ()
    | _ ->
      (try Printexc.print
             (fun l -> ctree_of_string l
                       |> eval_ctree
                       |> CanonicaltreeFlavor.to_string
                       |> print_endline)
             line
       with _ -> printf "got some error. try again pls.\n");
      print_flush (); loop (); in
  loop ()
