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

let rec eval_canonical ctree =
  let open Genslib in
  let open Gensl.Basetypes in
  let open Gensl.Canonicaltree in
  match ctree with
  | CAtom _ -> ctree
  | CForm {ckwd = _; cpos = (CAtom (SymbolAtom op)) :: args} ->
    let qargs =
      List.map
        (fun arg ->
           eval_canonical arg
           |> (function
               | CAtom (NumericAtom (x, "")) -> Q.of_string x
               | _ as arg ->
                 invalid_arg' "not a number: @a"
                   Intf.CanonicaltreeFlavor.pp arg))
        args in
    (match op with
     | "add" ->
       let result = foldl Q.((+)) Q.zero qargs in
       CAtom (NumericAtom (Q.to_string result, ""))
     | "sub" ->
       let result =
         match qargs with
         | x :: rest -> Q.(x - foldl (+) zero rest)
         | _ as arg ->
           invalid_arg' "arity mismatch: @a" Intf.CanonicaltreeFlavor.pp arg in
       CAtom (NumericAtom (Q.to_string result, ""))
     | _ -> CAtom (SymbolAtom "unknown op"))
  | _ -> [%noimplval]
