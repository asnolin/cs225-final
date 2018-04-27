(*Final Project by Andrew Nolin and Jared Wiggett *)
(*Course: UVM CS 225 spring 2018*)

(*to compile: make project*)
(*TODO for checkpoint:
        * small step semantics for 9.1 simply typed lambda calc
        * type checker for simply typed lambda calc
        * small step semantics for untyped lambda calc
        * unique_vars from ec1 to do substitution unique_vars should call free_vars
*)

(*Util and StringSetMap modules are from davdar*)
open Util
open StringSetMap


(*

exception NOT_FOUND

(*types*)
type ty =
	|Bool
	|Nat
	|Fun of ty * ty
(*expressions*)
type exp = 
        |If of exp * exp * exp
	|Zero
	|Succ of exp
	|Pred of exp
	|IsZero of exp
        |Var of string
	|Lam of string * ty * exp
        |App of exp * exp

(*values*)
type natval =
	|VZero
	|VSucc of natval

type value =
	|VTrue
	|VFalse
	|VNat of natval
	|VLambda of value * ty * exp

(*small step semantics*)
type result =
	|Val of value
	|Step of exp
	|Stuck

let rec step (e : exp) : result = match e with
	|If(e1,e2,e3) -> raise TODO
	|Zero -> raise TODO
	|Succ(e1) -> raise TODO
	|Pred(e1) -> raise TODO
	|IsZero(e1) -> raise TODO
	|Var(x) -> raise TODO
	|Lam (x,t1,e1)-> raise TODO
	|App(e1,e2) -> begin match step e1 with
        (*begin matching for e1*)
                |Val(x) -> begin match step e2 with
                (*begin matching for e2*)
                        |Val(y) -> raise TODO (*could be stuck??*)
                        |Step(e2') -> Step(App(e1, e2'))
                        |Stuck -> Stuck
                end (*end matching for e2*)
                |Step(e1') -> Step(App(e1', e2))
                |Stuck-> begin match e1 with
                (*begin matching for redux *)
                         |Lam(x, t1, e11) -> raise TODO
                         |_ -> raise TODO
                end (*end matching for redux *)
        end (*end matching for e1*)


(*free vars function*)
let rec free_vars (e0: exp) : string_set = match e0 with
        |If(e1,e2,e3) -> StringSet.union (StringSet.union (free_vars e1) (free_vars e2)) (free_vars e3)
        |Zero -> StringSet.empty
        |Succ(x) -> raise TODO
        |Pred(x) -> raise TODO
        |IsZero(x) -> raise TODO
        |Var(x) -> raise TODO
        |Lam(x,y,z) -> raise TODO
        |App(e1, e2) -> raise TODO

*)


(*redoing the program starting from defs on page 72 of tapl*)


type term = 
        |Var of string
        |Lam of string * term
        |App of term * term



type value = 
        |AbstrVal of term

let term_of_val (v0 : value) : term  = match v0 with
|AbstrVal(t) -> t

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
        if not (StringSet.equal StringSet.empty (free_vars t))
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
                                then raise TODO (*I think this needs alpha conversion*)
                               else Lam(y, (subst x v t2))
                        |App(t2, t3) -> App((subst x v t2), (subst x v t3)) 
                        end
                        else t
        else t







let rec eval (t0 : term) : result = match t0 with
        |Var(x) -> Val(AbstrVal(Var(x)))
        |Lam(x,t1) -> Stuck
        |App(t1, t2) -> begin match t1 with
                |Lam(x,t1') -> begin match t2 with
                (*matching for E-Appabs*)
                        |Var(y) -> raise TODO(*Eval(reduce x AbstrVal(t2) t1' )  *)
                        |_ -> raise TODO
                end
                |_ -> raise TODO
        end


(*testing*)
type test_result =
        |Passed
        |Failed
        |Todo

(*from ec1*)
type test_block =
        TestBlock : string * ('a * 'b) list * ('a -> 'b) * ('a -> string) * ('b -> string) -> test_block

let tests = 
        let redux : term = App(Lam("x",Var("x")),Var("y")) in
        let redux_ans : value = AbstrVal(Var("y")) in
        redux
(*end testing*)
