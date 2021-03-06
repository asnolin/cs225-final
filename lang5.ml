open Util
open StringSetMap
open Arith

type ty =
        |Bool
        |Unit
        |Fun of ty * ty
        |Number
        |Exception of ty
        [@@deriving show]

exception TYPE_ERROR

type term =
        (*bool terms*)
        |True
        |False
        |If of term * term * term
        (*typed lambda calc terms*)
        |Var of string
        |Lam of string * ty * term
        |App of term * term
        |Error of ty
        |TryWith of term * term
        (*number terms*)
        |Nan
        |Zero
        |Succ of term
        |Pred of term
        |Add of term * term
        |Sub of term * term
        |Mult of term * term
        |Div of term * term
        |Mod of term * term
        |Sqrt of term      

        |Raise of term

        [@@deriving show]
        
type value =
        |VTrue
        |VFalse
        |Res of term
        |DIV_BY_0
        |IMAGINARY
        |VLam of string * ty * term
        [@@deriving show]

type result = 
        |Stuck
        |Step of term
        |Val of value
        |RError of ty
        |RRaise of term
        [@@deriving show]

type tenv = ty string_map

let rec term_of_val (v : value) : term = match v with
        |VTrue -> True
        |VFalse -> False
        |VLam(x,ty,t) -> Lam(x,ty,t)
        |Res(n) -> n
        |DIV_BY_0 -> Nan
        |IMAGINARY -> Nan
(*free vars*)
let rec free_vars (t0 : term) : string_set = match t0 with
  | True -> StringSet.empty
  | False -> StringSet.empty
  | If(t1,t2,t3) -> StringSet.union (StringSet.union (free_vars t1) (free_vars t2)) (free_vars t3)
  | Var(x) -> StringSet.of_list [x]
  | Lam(x,ty,t) -> StringSet.remove x (free_vars t)
  | App(t1,t2) -> StringSet.union (free_vars t1) (free_vars t2)
  | Error(ty) -> StringSet.empty
  | TryWith(t1,t2) -> StringSet.union (free_vars t1) (free_vars t2)
  |Nan -> StringSet.empty
  |Zero -> StringSet.empty
  |Succ(n) -> free_vars n
  |Pred(n) -> free_vars n
  |Add(n1, n2) -> StringSet.union (free_vars n1) (free_vars n2)
  |Sub(n1, n2) -> StringSet.union (free_vars n1) (free_vars n2)
  |Mult(n1, n2) -> StringSet.union (free_vars n1) (free_vars n2)
  |Div(n1, n2) -> StringSet.union (free_vars n1) (free_vars n2)
  |Mod(n1, n2) -> StringSet.union (free_vars n1) (free_vars n2)
  |Sqrt(n) -> free_vars n
  | Raise(t1) -> free_vars t1



(* Enforces global uniqueness of variables. from ec1 *)
let unique_vars (t : term) : term =
  let new_var (iO : int option) (x : string) : string = match iO with
      | None -> x
      | Some(i) -> x ^ string_of_int i
  in
  let next_var (iO : int option) : int option = match iO with
      | None -> Some(1)
      | Some(i) -> Some(i+1)
  in
  let rec rename_var_r
    (iO : int option)
    (x : string)
    (g : string_set)
    : string * string_set =
      let x' = new_var iO x in
      if StringSet.mem x' g
      then rename_var_r (next_var iO) x g
      else (x',StringSet.add x' g)
  in
  let rename_var = rename_var_r None in
  let rec unique_vars_r (t0 : term) (env : string string_map) (g : string_set) : term * string_set = 
          match t0 with
      |Nan -> (Nan,g)
      |Zero ->(Zero,g)
      |Succ(n) -> unique_vars_r n env g
      |Pred(n) -> unique_vars_r n env g
      |Add(n1,n2) -> 
                let (n1',g') = unique_vars_r n1 env g in
                let (n2',g') = unique_vars_r n1 env g in
                (Add(n1',n2'),g')           
      |Sub(n1,n2) ->
                let (n1',g') = unique_vars_r n1 env g in
                let (n2',g') = unique_vars_r n1 env g in
                (Sub(n1',n2'),g') 
      |Mult(n1,n2) ->
                let (n1',g') = unique_vars_r n1 env g in
                let (n2',g') = unique_vars_r n1 env g in
                (Mult(n1',n2'),g') 
      |Div(n1,n2) ->
                let (n1',g') = unique_vars_r n1 env g in
                let (n2',g') = unique_vars_r n1 env g in
                (Div(n1',n2'),g') 
      |Mod(n1,n2) -> 
                let (n1',g') = unique_vars_r n1 env g in
                let (n2',g') = unique_vars_r n1 env g in
                (Mod(n1',n2'),g')      
      |Sqrt(n) -> unique_vars_r n env g
      | Error(ty) -> (Error(ty),g)
      | TryWith(t1,t2) -> 
          let (t1',g') = unique_vars_r t1 env g in
          let (t2',g'') = unique_vars_r t2 env g' in
          (TryWith(t1',t2'),g'')
      | Raise(t1) -> let (t1',g') = unique_vars_r t1 env g in
              (Raise(t1'),g)
      | True -> (True,g)
      | False -> (False,g)
      | If(t1,t2,t3) -> 
                let (t1',g'1) = unique_vars_r t1 env g in
                let (t2',g'2) = unique_vars_r t2 env g'1 in
                let (t3',g'3) = unique_vars_r t3 env g'2 in
                (If(t1',t2',t3'),g'3)
      | Var(x) -> (Var(StringMap.find x env),g)
      | Lam(x,ty,t) ->
          let (x',g') = rename_var x g in
          let (e',g'') = unique_vars_r t (StringMap.add x x' env) g' in
          (Lam(x',ty,t),g'')
      | App(e1,e2) ->
          let (e1',g') = unique_vars_r e1 env g in
          let (e2',g'') = unique_vars_r e2 env g' in
          (App(e1',e2'),g'')
      (* New cases *)
  in
  let initial_env (ss : string_set) =
    List.fold_right (fun x -> StringMap.add x x) (StringSet.elements ss) StringMap.empty
  in
  let fvs : string_set = free_vars t in
  let (e',_) = unique_vars_r t (initial_env fvs) fvs in
  e'

(*from ec1*)
let rec subst_r (x : string) (t2 : term)(t10 : term) : term = match t10 with
    |True -> t10
    |False -> t10
    |If(t11,t12,t13) -> If(subst_r x t2 t11,subst_r x t2 t12,subst_r x t2 t13)
    |Var(y) -> if x = y then t2 else t10
    |Lam(y,ty,t1) -> Lam(y,ty,subst_r x t2 t1)
    |App(t11, t12) -> App(subst_r x t2 t11,subst_r x t2 t12)
    |Error(ty) -> t10
    |TryWith(t11,t12) -> TryWith(subst_r x t2 t11,subst_r x t2 t12)
    |Zero -> t10
    |Nan -> t10
    |Succ(n) -> Succ(subst_r x t2 n)
    |Pred(n) -> Pred(subst_r x t2 n)
    |Add(n1,n2) -> Add(subst_r x t2 n1, subst_r x t2 n2)
    |Sub(n1,n2) ->Sub(subst_r x t2 n1, subst_r x t2 n2)
    |Mult(n1,n2) -> Mult(subst_r x t2 n1, subst_r x t2 n2)
    |Div(n1,n2) -> Div(subst_r x t2 n1, subst_r x t2 n2)
    |Mod(n1,n2) -> Mod(subst_r x t2 n1, subst_r x t2 n2)
    |Sqrt(n) -> Sqrt(subst_r x t2 n)
    |Raise(t11) -> Raise(subst_r x t2 t11)


(*when App(Lam(x.t),t)[x->v]t from ec1 *)
let rec subst(x : string) (ty1 : ty) (t2 : term) (t1 : term) : term  = match unique_vars(App(Lam(x,ty1,t1),t2)) with
    |App(Lam(x',ty,t1'),t2') -> subst_r x' t2' t1'
    |_->raise IMPOSSIBLE
        
let rec num_of_term (t : term) : number = match t with
        |Nan -> Nan
        |Zero -> Zero
        |Succ(t') -> Succ(num_of_term t')
        |Pred(t') -> Pred(num_of_term t')
        |Add(t1,t2) ->Add(num_of_term t1,num_of_term t2)
        |Sub(t1,t2) ->Sub(num_of_term t1, num_of_term t2)
        |Mult(t1,t2) ->Mult(num_of_term t1,num_of_term t2)
        |Div(t1,t2) ->Div(num_of_term t1, num_of_term t2)
        |Mod(t1,t2) ->Mod(num_of_term t1, num_of_term t2)
        |Sqrt(t') -> Sqrt(num_of_term t')
        |_ -> Nan
(*step*)    (*from ec1 with our modifications*)
let rec step (t0 : term) : result = match t0 with
  |Nan -> RError(Number)
  |Zero -> Val(Res(Zero))
  |Succ(n) -> Val(Res(t0))
  |Pred(n) -> Val(Res(t0))
  |Add(n1,n2) -> raise TODO
  |Sub(n1,n2) -> raise TODO
  |Mult(n1,n2) -> raise TODO
  |Div(n1,n2) -> raise TODO
  |Mod(n1,n2) -> raise TODO
  |Sqrt(n) -> raise TODO
  (* λx:τ.e  ∈  val *)
  (* Our Definitions *)
  | True -> Val(VTrue)
  | False -> Val(VFalse)
  (* E-IfTrue, E-IfFalse p34 *)
  | If(t1,t2,t3) -> begin match t1 with
        |True -> Step(t2)
        |False -> Step(t3)
        |_ -> raise TYPE_ERROR
        end
  (* Our Definition *)
  | Lam(x,ty,t) -> Val(VLam(x,ty,t))
  (* App Evaluations p103,172,175 *)
  | App(t1,t2) -> begin match step t1 with
    | Val(v1) -> begin match step t2 with
      | Val(v2) -> begin match v1 with
        (* —————————————————————(β)
         * (λx:τ.e)v —→ [x ↦ v]e
         *)
        | VTrue -> Stuck
        | VFalse -> Stuck
        | VLam(x,ty,t) -> Step(subst x ty (term_of_val v2) t)
        |DIV_BY_0 -> raise TODO
        |IMAGINARY -> raise TODO
        |Res(n) -> raise TODO
        end
      (*   e₂ —→ e₂′
       * —————————————
       * v₁ e₂ —→ v₁ e₂′
       *)
      | Step(t2') -> Step(App(t1,t2'))
      | Stuck -> begin match v1 with
        |VLam(x,ty,t) -> Step(subst x ty t2 t)
        |_ -> Stuck
        end
      (* E-AppErr2 p172*)
      | RError(ty) -> RError(ty)
      (* E-AppRaise2 p175 *)
      | RRaise(t) -> begin match step t with
                |Val(v1) -> Step(t2)
                |_ -> Stuck
      end
      end
    (*    e₁ —→ e₁′
     * ———————————————
     * e₁ e₂ —→ e₁′ e₂
     *)
    | Step(t1') -> Step(App(t1',t2))
    (* Our Definition *)
    | Stuck -> Stuck
    (* E-AppErr1 p172 *)
    | RError(ty) -> RError(ty)
    (* E-AppRaise1 p175 *)
    | RRaise(t) -> begin match step t with
                |Val(v1) -> step t1
                |_ -> Stuck
    end
    end
  (* Our Definition *)
  |Var(x) -> Stuck
  |Error(ty) -> RError(ty)
  (* Try Evaluation p172,174,175 *)
  |TryWith(t1,t2) -> 
        begin match step t1 with
        (* E-TryV p174 *)
        |Val(v1) -> Step(t1)
        (* E-Try p174 *)
        |Step(t1') -> Step(TryWith(t1',t2))
        (* E-TryError p174 *)
        |RError(ty) -> Step(t2)
        (* E-TryRaise p175 *)
        |RRaise(t1') -> begin match step t1' with
                |Val(v1) -> Step(App(t2, t1'))
                |_ -> Stuck
        end
        |_ -> Stuck
        end
  (* Raise Evaluation p175 *)
  |Raise(t) -> begin match step t with
        (* E-Raise p175 *)
        |Step(t') -> Step(Raise(t'))
        (* E-RaiseRaise p175 *)
        |RRaise(t') -> begin match step t' with
                |Val(v1) -> RRaise(t')
                |_ -> Stuck
        end
        |_ -> Stuck
  end

(* Pieces taken from Darais' hw5.ml *)
let rec infer (g : tenv) (t : term) : ty = match t with
        |Nan -> raise TODO
        |Zero -> raise TODO
        |Succ(n) -> raise TODO
        |Pred(n) -> raise TODO
        |Add(n1,n2) -> raise TODO
        |Sub(n1,n2) -> raise TODO
        |Mult(n1,n2) -> raise TODO
        |Div(n1,n2) -> raise TODO
        |Mod(n1,n2) -> raise TODO
        |Sqrt(n) -> raise TODO
        |True -> Bool
        |False -> Bool
        |If(t1,t2,t3) -> 
                let ty1 = infer g t1 in
                let ty2 = infer g t2 in
                let ty3 = infer g t3 in
                if not (ty1 = Bool) then raise TYPE_ERROR else
                if not (ty2 = ty3) then raise TYPE_ERROR else
                ty2
        (* T-Var *)
        |Var(x) -> StringMap.find x g
        (* T-Abs p103 *)
        |Lam(x,ty,t1) -> let ty2 = infer (StringMap.add x ty g) t1 in
                Fun(ty,ty2)
        (* T-App p103 *)
        |App(t1,t2) -> let ty = infer g t1 in
                begin match ty with
                |Fun(ty1,ty2) ->
                        if ty1 = infer g t2 then ty2
                        else raise TYPE_ERROR
                |_ -> raise TYPE_ERROR
                end
        (* T-Error p172 *)
        |Error(ty) -> ty
        (* T-Try p175 *)
        |TryWith(t1,t2) -> 
                let ty1 = infer g t1 in
                let ty2 = infer g t2 in
                begin match ty2 with
                        |Fun(ty21, ty22) -> 
                                        if not (ty1 = ty22) then raise TYPE_ERROR
                                        else begin match ty21 with
                                                |Exception(ty21') -> ty1
                                                |_ -> raise TYPE_ERROR
                                        end
                        |_ -> raise TYPE_ERROR
                end
        (* T-Exn p175 *)
        |Raise(t1) -> let ty = infer g t1 in
                begin match ty with
                        |Exception(ty') -> ty'
                        |_ -> raise TYPE_ERROR
                end

(*testing *)
let tests = 
    (*lambda calc tests*)
    let lambda : term = Lam("x",Unit,Var("x")) in 
    let lambda_ans : result = Val(VLam("x",Unit,Var("x"))) in
    let app : term = App(Lam("x",Unit, Var("x")),Var("y")) in
    let app_ans : result = Step(Var("y")) in
    let appOfVars : term = App(Var("x"),Var("y")) in
    let appOfVars_ans : result = Stuck in 
    (*infer tests*)

let step_test : Util.test_block = 
    TestBlock
    ("Lang5 subst",
    [lambda, lambda_ans
    ;app, app_ans
    ;appOfVars, appOfVars_ans
    ],step, (=),show_term ,show_result)in
run_tests[step_test]

(*
let infer_test : Util.test_block =
    TestBlock
    ("Lang5 infer",
    [

    ],infer, (=), show_term, show_ty)in
run_tests[infer_test] *)
