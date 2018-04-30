(*Final Project by Andrew Nolin and Jared Wiggett *)
(*Course: UVM CS 225 spring 2018*)

(*to compile: make project*)
(*TODO for checkpoint:
        * small step semantics for 9.1 simply typed lambda calc
        * type checker for simply typed lambda calc
        * small step semantics for untyped lambda calc
        * unique_vars from ec1 to do substitution unique_vars should call free_vars
        * arithmatic terms to demonstrate exceptions
*)

(*Util and StringSetMap modules are from hw5 and hw3*)
open Util
open StringSetMap

type term = 
        |Var of string
        |Lam of string * term
        |App of term * term



type value = 
        |AbstrVal of term

let term_of_val (v0 : value) : term = match v0 with
|AbstrVal(t) -> t

let val_of_term (t0 : term) : value = match t0 with
|_ -> AbstrVal(t0)

type result=
        |Stuck
        |Val of value
        |Eval of term

(*from ec1*)
let rec free_vars (t0 : term) : string_set = match t0 with
        |Var(x) -> StringSet.of_list [x]
        |Lam(x,t) -> StringSet.remove x (free_vars t)
        |App(t1, t2) -> StringSet.union (free_vars t1) (free_vars t2)

    



(*[x -> v]t*)
let rec subst (x : string) (v : value) (t : term) : term =
        (*if FV(t) != {}*)
        if not (StringSet.equal StringSet.empty (free_vars t))
                (* then if x is a member of FV(t)*)
                then if StringSet.mem x (free_vars t)
                        then begin match t with
                        (*for every free occurance of x in t, replace x with v*)
                        |Var(y) -> 
                                if x = y
                                then t
                                else term_of_val v

                        |Lam(y, t2) -> 
                                if x = y
                                then t
                                else if StringSet.mem y (free_vars(term_of_val( v)))
                                then raise TODO (*I think this needs alpha conversion here*)
                                else Lam(y, (subst x v t2))

                        |App(t2, t3) -> App((subst x v t2), (subst x v t3)) 
                        end

                        else t (*end if x is a member of FV(t)*)
        else t(*end if FV(t) != {}*)






(*does 1 step of evaluation*)
let rec eval (t0 : term) : result = match t0 with
        |Var(x) -> Val(AbstrVal(Var(x)))
        |Lam(x,t1) -> Val(AbstrVal(Lam(x,t1)))
        |App(t1, t2) -> begin match t1 with
                |Lam(x,t1') -> begin match t2 with
                (*matching for E-Appabs*)
                        |Var(y) -> 
                                let s = subst x (val_of_term t2) t1' in
                                Eval(s)
                        |_ -> raise TODO
                end
                |_ -> raise TODO
        end




(*testing*)
type test_result =
        |Passed
        |Failed
        |Todo

let tests =
       (*ID function*) 
        let redux : term = App(Lam("x",Var("x")),Var("y")) in
        let redux_ans : value = AbstrVal(Var("y")) in
        (*variable eval*)
        let absVal : term = Var("z")in
        let absVal_ans : value = AbstrVal(Lam("x",Var("x")))in
        (**)
        (**)
redux

(*end testing*)
