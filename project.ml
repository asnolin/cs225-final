(*Final Project by Andrew Nolin and Jared Wiggett *)
(*Course: UVM CS 225 spring 2018*)
(*TODO for checkpoint:
        * small step semantics for 9.1 simply typed lambda calc
        * type checker for simply typed lambda calc
        * CONFIG MERLIN WITH .merlin file, needs source and build paths? packages?
*)

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


type variable = 
        |Var of string


type term = 
        |Var of variable
        |Lam of variable * term
        |App of term * term



type value = 
        |AbstrVal of term


type result=
        |Stuck
        |Val of value
        |Eval of term

let rec reduce (x : variable) (v : value) (t: term) : term = match x with
        |Var(x) -> raise TODO


let rec eval (t0 : term) : result = match t0 with
        |Var(x) -> Val(AbstrVal(Var(x)))
        |Lam(x,t1) -> Stuck
        |App(t1, t2) -> begin match t1 with
                |Lam(x,t1') -> begin match t2 with
                (*matching for E-Appabs*)
                        |Var(y) -> raise TODO (*  Eval(reduce ( x t2 t1'))  *)
                        |_ -> raise TODO
                end
                |_ -> raise TODO
        end
