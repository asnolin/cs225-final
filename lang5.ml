open Util
open StringSetMap

type term =
        |Var of string
        |Lam of string * term
        |App of term * term
        [@@deriving show]
type value =
        |VLam of string * term
        [@@deriving show]

type result = 
        |Stuck
        |Step of term
        |Val of value
        [@@deriving show]

(*free vars*)

(*[x->v]t*)

(*step*)
let rec step (t :term) : result = match t with 
    |Var(s) -> Stuck
    |Lam(s,t') -> Stuck
    |App(t1,t2) -> begin match step t1 with
        |Stuck -> Stuck
        |Val(t1') -> begin match step t2 with
            |_-> raise TODO
            end
        |Step(t1') -> raise TODO
        end
(*testing *)
let tests = 
    let lambda : term = Lam("x",Var("x")) in 
    let lambda_ans : result = Stuck in

let lang_test : Util.test_block = 
    TestBlock
    ("Lang5"
   ,[
lambda, lambda_ans

    ],step, (=),show_term ,show_result)in
run_tests[lang_test]
