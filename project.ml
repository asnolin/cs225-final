(*Final Project by Andrew Nolin and Jared Wiggett *)
(*Course: UVM CS 225 spring 2018*)

(*to compile: make project*)
(*TODO for checkpoint:
        * small step semantics for 9.1 simply typed lambda calc
        * type checker for simply typed lambda calc
        * small step semantics for untyped lambda calc
        * unique_vars from ec1 to do substitution unique_vars should call free_vars
        * arithmatic terms to demonstrate exceptions
        * types need [@@deriving show {with_path = true/false}]
        * Mult needs pattern matching on n1
        * complete Div
*)

(*Util and StringSetMap modules are from hw5 and hw3*)
open Util
open StringSetMap
type ty =
        |Bool
        |Fun of ty * ty
        |TError
        [@@deriving show]

type term = 
	|True
	|False
	|If of term * term * term
        |Var of string
        |Lam of string * ty * term
        |App of term * term
	|Error of ty
        [@@deriving show]

type value = 
        |VTrue
	|VFalse
	|AbstrVal of term
        [@@deriving show]
let term_of_val (v0 : value) : term = match v0 with
	|AbstrVal(t) -> t
	|VTrue -> True
	|VFalse -> False

let val_of_term (t0 : term) : value = match t0 with
	|True -> VTrue
	|False -> VFalse
	|_ -> AbstrVal(t0)

type result=
        |Stuck
        |Val of value
        |Eval of term
	|RError
        [@@deriving show]
(*from ec1*)
let rec free_vars (t0 : term) : string_set = match t0 with
	|True -> StringSet.empty
	|False -> StringSet.empty
	|If(e1,e2,e3) ->
		StringSet.union (StringSet.union (free_vars e1) (free_vars e2)) (free_vars e3)
        |Var(x) -> StringSet.of_list [x]
        |Lam(x,ty,t) -> StringSet.remove x (free_vars t)
        |App(t1, t2) -> StringSet.union (free_vars t1) (free_vars t2)
	|Error(ty) -> StringSet.empty

    



(*[x -> v]t*)
let rec subst (x : string) (v : value) (t : term) : term =
        (*if FV(t) != {}*)
        if not (StringSet.equal StringSet.empty (free_vars t))
                (* then if x is a member of FV(t)*)
                then if StringSet.mem x (free_vars t)
                        then begin match t with
                        (*for every free occurance of x in t, replace x with v*)
                        |True -> True
			|False -> False
			|If(e1,e2,e3) -> If((subst x v e1), (subst x v e2), (subst x v e3))
			|Var(y) -> 
                                if x = y
                                then t
                                else term_of_val v

                        |Lam(y,ty, t2) -> 
                                if x = y
                                then t
                                else if StringSet.mem y (free_vars(term_of_val( v)))
                                then raise TODO (*I think this needs alpha conversion here*)
                                else Lam(y,ty, (subst x v t2))

                        |App(t2, t3) -> App((subst x v t2), (subst x v t3)) 
                        |Error(ty) -> Error(ty)
			end

                        else t (*end if x is a member of FV(t)*)
        else t(*end if FV(t) != {}*)






(*does 1 step of evaluation*)
let rec eval (t0 : term) : result = match t0 with
	|True -> Val(VTrue)
	|False -> Val(VFalse)
	|If(e1,e2,e3) -> begin match e1 with
                |True -> Eval(e2)
		|False -> Eval(e3)
		|_ -> RError
        end
        |Var(x) -> Val(AbstrVal(Var(x)))
        |Lam(x,ty,t1) -> Val(AbstrVal(Lam(x,ty,t1)))
        |App(t1, t2) -> begin match t1 with
                |Lam(x,ty,t1') -> begin match t2 with
                (*matching for E-Appabs*)
                        |Var(y) -> 
                                let s = subst x (val_of_term t2) t1' in
                                Eval(s)
                        |_ -> raise TODO
                end
		|Error(ty) -> RError
                |_ -> begin match t1 with
                        |Error(ty) -> RError
                        |_->raise TODO
                end
        end
        |Error(ty) -> RError



(*testing*)
type test_result =
        |Passed
        |Failed
        |Todo

let tests =
        (*ID function*) 
        let redux : term = App(Lam("x",Bool,Var("x")),Var("y")) in
        let redux_ans : result = Val(AbstrVal(Var("y"))) in
        (*variable eval*)
        let absVal : term = Var("z")in
        let absVal_ans : result = Val(AbstrVal(Lam("x",Bool,Var("x")))) in
        (*the test block*)
        let step_test : Util.test_block = 
                TestBlock
                ("Step",
                [redux  ,redux_ans
                ;absVal ,absVal_ans
                ],eval, (=), show_term, show_result) in
        run_tests[step_test];


        
(*end testing*)


