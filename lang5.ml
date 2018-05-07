open Util
open StringSetMap

type ty =
        |Bool
        |Unit
        |Fun of ty * ty
        [@@deriving show]

exception TYPE_ERROR

type term =
        |True
        |False
        |If of term * term * term
        |Var of string
        |Lam of string * ty * term
        |App of term * term
        [@@deriving show]
        
type value =
        |VLam of string * ty * term
        [@@deriving show]

type result = 
        |Stuck
        |Step of term
        |Val of value
        [@@deriving show]

type tenv = ty string_map

let rec term_of_val (v : value) : term = match v with
        |VLam(x,ty,t) -> Lam(x,ty,t)

(*free vars*)
let rec free_vars (t0 : term) : string_set = match t0 with
  | True -> StringSet.empty
  | False -> StringSet.empty
  | If(t1,t2,t3) -> StringSet.union (StringSet.union (free_vars t1) (free_vars t2)) (free_vars t3)
  | Var(x) -> StringSet.of_list [x]
  | Lam(x,ty,t) -> StringSet.remove x (free_vars t)
  | App(t1,t2) -> StringSet.union (free_vars t1) (free_vars t2)


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

(*when App(Lam(x.t),t)[x->v]t from ec1 *)
let rec subst(x : string) (ty1 : ty) (t2 : term) (t1 : term) : term  = match unique_vars(App(Lam(x,ty1,t1),t2)) with
    |App(Lam(x',ty,t1'),t2') -> subst_r x' t2' t1'
    |_->raise IMPOSSIBLE


(*step*)    (*from ec1, stripped down to untyped lambda calc*)
let rec step (t0 : term) : result = match t0 with
  (* λx:τ.e  ∈  val *)
  | True -> Stuck
  | False -> Stuck
  | If(t1,t2,t3) -> begin match t1 with
        |True -> Step(t2)
        |False -> Step(t3)
        |_ -> raise TYPE_ERROR
        end
  | Lam(x,ty,t) -> Val(VLam(x,ty,t))
  | App(t1,t2) -> begin match step t1 with
    | Val(v1) -> begin match step t2 with
      | Val(v2) -> begin match v1 with
        (* —————————————————————(β)
         * (λx:τ.e)v —→ [x ↦ v]e
         *)
        | VLam(x,ty,t) -> Step(subst x ty (term_of_val v2) t)
        end
      (*   e₂ —→ e₂′
       * —————————————
       * v₁ e₂ —→ v₁ e₂′
       *)
      | Step(t2') -> Step(App(t1,t2'))
      | Stuck -> Stuck
      end
    (*    e₁ —→ e₁′
     * ———————————————
     * e₁ e₂ —→ e₁′ e₂
     *)
    | Step(t1') -> Step(App(t1',t2))
    | Stuck -> Stuck
    end
  |Var(x) -> Stuck

let rec infer (g : tenv) (t : term) : ty = match t with
        |True -> Bool
        |False -> Bool
        |If(t1,t2,t3) -> 
                let ty1 = infer g t1 in
                let ty2 = infer g t2 in
                let ty3 = infer g t3 in
                if not (ty1 = Bool) then raise TYPE_ERROR else
                if not (ty2 = ty3) then raise TYPE_ERROR else
                ty2
        |Var(x) -> StringMap.find x g
        |Lam(x,ty,t1) -> let ty2 = infer (StringMap.add x ty g) t1 in
                Fun(ty,ty2)
        |App(t1,t2) -> let ty = infer g t1 in
                begin match ty with
                |Fun(ty1,ty2) ->
                        if ty1 = infer g t2 then ty2
                        else raise TYPE_ERROR
                |_ -> raise TYPE_ERROR
                end

(*testing *)
let tests = 
    let lambda : term = Lam("x",Unit,Var("x")) in 
    let lambda_ans : result = Val(VLam("x",Unit,Var("x"))) in
    (*TODO more tests*)

let lang_test : Util.test_block = 
    TestBlock
    ("Lang5"
   ,[
lambda, lambda_ans

    ],step, (=),show_term ,show_result)in
run_tests[lang_test]
