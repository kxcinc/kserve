open Gensl_eval.Eval

let eval s = eval_ctree (ctree_of_string s)

(* add tests *)
let%test _ = eval "(add)" = eval "0"
let%test _ = eval "(add 7)" = eval "7"
let%test _ = eval "(add 999999)" = eval "999999"
let%test _ = eval "(add -21)" = eval "-21"
let%test _ = eval "(add 2.0)" = eval "2"
let%test _ = eval "(add 2 3)" = eval "5"
let%test _ = eval "(add 2 3 100)" = eval "105"
let%test _ = eval "(add 2.0 3 100)" = eval "105"
let%test _ = eval "(add 2.0 3 100 -0.8)" = eval "104.2"
let%test _ = eval "(add (add 1 2 (add 3 4) (add 5 6) 7) 8 (add 9 10))" = eval "55"

(* sub tests *)
let%test _ = eval "(sub 7)" = eval "7"
let%test _ = eval "(sub 999999)" = eval "999999"
let%test _ = eval "(sub -21)" = eval "-21"
let%test _ = eval "(sub 2.0)" = eval "2"
let%test _ = eval "(sub 2 3)" = eval "-1"
let%test _ = eval "(sub 2 3 100)" = eval "-101"
let%test _ = eval "(sub 2.0 3 100)" = eval "-101"
let%test _ = eval "(sub 2.0 3 100 -0.8)" = eval "-100.2"
let%test _ = eval "(sub (sub 1 2 (sub 3 4) (sub 5 6) 7) 8 (sub 9 10))" = eval "-13"
let%test _ = eval "(sub (add 1 2 (sub 3 4) (add 5 6) 7) 8 (add 9 10))" = eval "-7"

(* mul tests *)
let%test _ = eval "(mul)" = eval "1"
let%test _ = eval "(mul 7)" = eval "7"
let%test _ = eval "(mul 999999)" = eval "999999"
let%test _ = eval "(mul -21)" = eval "-21"
let%test _ = eval "(mul 2.0)" = eval "2"
let%test _ = eval "(mul 2 3)" = eval "6"
let%test _ = eval "(mul 2 3 100)" = eval "600"
let%test _ = eval "(mul 2.0 3 100)" = eval "600"
let%test _ = eval "(mul 2.0 3 100 -0.8)" = eval "-480"
let%test _ = eval "(mul (mul 1 2 (mul 3 4) (mul 5 6) 7) 8 (mul 9 10))" = eval "3628800"

(* div tests *)
let%test _ = eval "(div 7)" = eval "7"
let%test _ = eval "(div 999999)" = eval "999999"
let%test _ = eval "(div -21)" = eval "-21"
let%test _ = eval "(div 2.0)" = eval "2"
let%test _ = eval "(div 2 3)" = eval "2/3"
let%test _ = eval "(div 2 3 100)" = eval "1/150"
let%test _ = eval "(div 2.0 3 100)" = eval "1/150"
let%test _ = eval "(div 2.0 3 100 -0.8)" = eval "-1/120"
let%test _ = eval "(div (div 1 2 (div 3 4) (div 5 6) 7) 8 (div 9 10))" = eval "1/63"
let%test _ = eval "(div (add 1 2 (sub 3 4) (mul 5 6) 7) 8 (mul 9 10))" = eval "13/240"
