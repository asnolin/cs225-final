(*Final Project by Andrew Nolin and Jared Wiggett *)
(*Course: UVM CS 225 spring 2018*)
(*TODO for checkpoint:
        * small step semantics for 9.1 simply typed lambda calc
        * type checker for simply typed lambda calc
*)
(*types*)
type ty =
	| Bool
	| Nat
	| Fun of ty * ty
(*expressions*)
type exp = 
        |True
        |False
        |Var of string
        |App of exp * exp
        |Lam of string * ty * exp
        |If of exp * exp * exp

(*values*)
type natval =
	| VZero
	| VSucc of natval

type value =
	| VTrue
	| VFalse
	| VNat of natval
	| VLambda of var * ty * exp


(*small step semantics*)
