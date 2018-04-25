(*Final Project by Andrew Nolin and Jared Wiggett *)
(*Course: UVM CS 225 spring 2018*)
(*TODO for checkpoint:
        * small step semantics for 9.1 simply typed lambda calc
        * type checker for simply typed lambda calc
*)
(*types*)
type ty = 
        |Bool
        |Fun of ty * ty

(*expressions*)
type exp = 
        |True
        |False
        |Var of string
        |App of exp * exp
        |Lam of string * ty * exp
        |If of exp * exp * exp



(*values*)


(*small step semantics*)
