(*Final Project by Andrew Nolin and Jared Wiggett *)
(*Course: UVM CS 225 spring 2018*)
(*TODO for checkpoint:
        * small step semantics for 9.1 simply typed lambda calc
        * type checker for simply typed lambda calc
*)
(*types*)
type ty =
	|Bool
	|Nat
	|Fun of ty * ty
(*expressions*)
type exp = 
        |True
        |False
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
	|True -> raise TODO
	|False -> raise TODO
	|If(e1,e2,e3) -> raise TODO
	|Zero -> raise TODO
	|Succ(e1) -> raise TODO
	|Pred(e1) -> raise TODO
	|IsZero(e1) -> raise TODO
	|Var -> raise TODO
	|Lam -> raise TODO
	|App(e1,e2) -> raise TODO


(*free vars function*)
let rec free_vars (e0: exp) : string_set = match e0 with
        |True -> StringSet.empty
        |False -> StringSet.empty
        |If(e1,e2,e3) -> StringSet.union (StringSet.union (free_vars e1) (free_vars e2)) (free_vars e3)
        |

