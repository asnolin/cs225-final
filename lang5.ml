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

let rec term_of_val (v : value) : term = match v with
        |VLam(x,t) -> Lam(x,t)

(*free vars*)
let rec free_vars (t0 : term) : string_set = match t0 with
  | Var(x) -> StringSet.of_list [x]
  | Lam(x,t) -> StringSet.remove x (free_vars t)
  | App(t1,t2) -> StringSet.union (free_vars t1) (free_vars t2)


(* Enforces global uniqueness of variables. *)
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
      | Var(x) -> (Var(StringMap.find x env),g)
      | Lam(x,t) ->
          let (x',g') = rename_var x g in
          let (e',g'') = unique_vars_r t (StringMap.add x x' env) g' in
          (Lam(x',t),g'')
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


(*when App(Lam(x.t),t)[x->v]t*)
let rec subst(s : string) (t1 : term) (t2 : term) : term  = match t1 with
    |_->raise TODO


(*step*)    (*from ec1, stripped down to untyped lambda calc*)
let rec step (t0 : term) : result = match t0 with
  (* λx:τ.e  ∈  val *)
  | Lam(x,t) -> Val(VLam(x,t))
  | App(t1,t2) -> begin match step t1 with
    | Val(v1) -> begin match step t2 with
      | Val(v2) -> begin match v1 with
        (* —————————————————————(β)
         * (λx:τ.e)v —→ [x ↦ v]e
         *)
        | VLam(x,t) -> Step(subst x (term_of_val v2) t)
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


(*testing *)
let tests = 
    let lambda : term = Lam("x",Var("x")) in 
    let lambda_ans : result = Val(VLam("x", Var("x"))) in
    (*TODO more tests*)

let lang_test : Util.test_block = 
    TestBlock
    ("Lang5"
   ,[
lambda, lambda_ans

    ],step, (=),show_term ,show_result)in
run_tests[lang_test]
