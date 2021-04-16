open Genslib
open Gensl.Basetypes

type ctree = Gensl.Canonicaltree.cdatum =
    | CAtom of atom
    | CForm of {
        ckwd : (ctree, ctree) assoc;
        cpos : ctree list;
      }

let ctree_of_string raw_gensl =
  let open Intf in
  raw_gensl
  |> Lexing.from_string
  |> Parsing.ParserTypes.pstate
  |> Parser.Default.read_top
  |> (function
      | Ok (toplevel, _) -> toplevel
      | _e -> failwith "parse error")
  |> Parsetreeflavor.to_canonicaltree

module Helpers = struct
  open Intf.CanonicaltreeFlavor

  let numeric_atom x = CAtom (NumericAtom (Q.to_string x, ""))

  let val_of_numeric = function
    | CAtom (NumericAtom (str, "")) -> Q.of_string str
    | datum -> invalid_arg' "invalid numeric: %a" pp datum

  let pp_atom ppf a = Format.fprintf ppf "%a" pp (mkatom a)
end open Helpers

let apply_primitive prim ~posargs ~kwdargs =
  let assert_no_kwdargs() = match kwdargs with
    | [] -> ()
    | _ -> invalid_arg' "prim %s takes no keyworded arguments" prim in
  let single_atom_posarg() =
    assert_no_kwdargs();
    match posargs with
    | [CAtom a] -> a
    | _ -> invalid_arg' "prim %s takes a single atom argument" prim in
  let handle_numeric_op f =
     assert_no_kwdargs();
     let posargs = posargs |&> val_of_numeric in
     f posargs |> numeric_atom in
  match prim with
  | "add" ->
     handle_numeric_op Q.(foldl (+) zero)
  | "sub" ->
     handle_numeric_op Q.(List.reduce (-))
  | "mul" ->
     handle_numeric_op Q.(foldl ( * ) one)
  | "div" ->
     handle_numeric_op Q.(List.reduce (/))
  | "numeric_of_string" ->
     (match single_atom_posarg() with
      | StringAtom str -> Q.of_string str |> numeric_atom
      | a -> invalid_arg' "%s: unexpected atom: %a" prim pp_atom a)
  | "string_of_numeric" ->
     (match single_atom_posarg() with
      | NumericAtom (str,suffix) -> CAtom (StringAtom (str^suffix))
      | a -> invalid_arg' "%s: unexpected atom: %a" prim pp_atom a)
  | _ -> invalid_arg' "unrecognized primitive: %s" prim

(* Big-step operational semantics *)
let rec eval_ctree ctree =
  let open Gensl.Basetypes in
  let open Intf.CanonicaltreeFlavor in
  match ctree with
  | CAtom (NumericAtom (_, "")) as x -> val_of_numeric x |> numeric_atom
  | CAtom _ -> ctree
  | CForm { cpos = (CAtom (SymbolAtom prim)) :: posargs ;
            ckwd = kwdargs } ->
     let posargs = posargs |&> eval_ctree in
     let kwdargs = kwdargs |&> (fun (k,v) -> eval_ctree k, eval_ctree v) in
     apply_primitive prim ~posargs ~kwdargs
  | CForm _ as x -> invalid_arg' "ctree_eval: unrecognized form %a" pp x
