type exp =
  | Var of string
  | IntLit of int
  | BoolLit of bool
  | Plus of exp * exp 
  | If of exp * exp * exp 
  | Fun of string * exp
  | App of exp * exp
  | Let of string * exp * exp

type tyvar = string
type ty = TInt | TBool | TArrow of ty * ty | TVar of tyvar
type tyenv = (string * ty) list
type tysubst = (tyvar * ty) list

let ext env x v = (x,v) :: env

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y,v)::tl -> if x=y then v 
    else lookup x tl

let theta0 = ([] : tysubst)

(* new_typevar : int -> ty * int ;; 型が間違っていたので修正 [2013/12/04] *)
let new_typevar n = 
  (TVar ("'a" ^ (string_of_int n)), n+1)

(* substitute : ty -> ty -> tyenv -> tyenv *)
let rec substitute tvar t te =
  List.map (fun (x,t2) ->
    if t2 = tvar then (x,t) else (x,t2))
  te



let rec occurs tx t =
  if tx = t then true 
  else 
    match t with
    | TArrow(t1,t2) -> (occurs tx t1) or (occurs tx t2)
    | _ -> false

(* subst_ty : tysubst -> ty -> ty *)
(* 代入thetaを型t に適用する *)
let rec subst_ty theta t =
  let rec subst_ty1 theta1 s = 
    match theta1 with
      |	[] -> TVar(s)
      | (tx,t1):: theta2 -> 
	  if tx = s then t1 
	  else subst_ty1 theta2 s
  in match t with
    |  TInt -> TInt
    | TBool -> TBool
    | TArrow(t2,t3) -> TArrow(subst_ty theta t2, subst_ty theta t3)
    | TVar(s) -> subst_ty1 theta s

(* subst_tyenv  : tysubst -> tyenv -> tyenv *)
(* 代入thetaを型環境 te に適用する *)
let subst_tyenv theta te =
  List.map (fun (x,t) -> (x, subst_ty theta t)) te
(* List.mapは OCaml における リストに対する iterator である *)

(* subst_eq : tysubst -> (ty * ty) list -> (ty * ty) list *)
(* 代入thetaを型の等式のリスト eql に適用する *)
let subst_eql theta eql =
  List.map (fun (t1,t2) -> (subst_ty theta t1, subst_ty theta t2))
	   eql

(* compose_subst : tysubst -> tysubst -> tysubst *)
(* 2つの代入を合成した代入を返す。theta1 が「先」でtheta2が「後」である *)
let rec compose_subst theta2 theta1 =
  let theta11 = 
    List.map (fun (tx,t) -> (tx, subst_ty theta2 t)) theta1
  in
    List.fold_left (fun tau -> fun (tx,t) -> 
		      try 
			let _ = lookup tx theta1 in
			  tau
		      with Failure(_) ->
			(tx,t) :: tau)
                   theta11
                   theta2
(* List.fold_left は、リストに対する iterator であり、
 * List.fold_left f x [a1;a2;a3] は  (f (f (f x a1) a2) a3) を返す
 *)

(* unify : (ty * ty) list -> tysubst *)
let unify eql =
  let rec solve eql theta =
    match eql with
      | [] -> theta
      | (t1,t2):: eql2 ->
	  if t1 = t2 then solve eql2 theta
	  else 
      begin
        match (t1,t2) with
        | (TArrow(t11,t12),TArrow(t21,t22))
          -> solve ((t11,t21)::(t12,t22)::eql2) theta
        | (TVar(s), _)
          -> if (occurs t1 t2) then failwith "unification failed"
          else solve (subst_eql [(s,t2)] eql2)
                    (compose_subst [(s,t2)] theta)
        | (_,TVar(s))
          -> if (occurs t2 t1) then failwith "unification failed"
          else solve (subst_eql [(s,t1)] eql2)
                    (compose_subst [(s,t1)] theta)
        | (_,_) -> failwith "unification failed"
      end
  in solve eql []

(* remove: (tyenv -> string -> tyenv) *)
let rec remove te x =
  match te with
  | [] -> []
  | (y,v)::tl -> if x=y then remove tl x
    else (y,v)::(remove tl x)

(* tinf2 : tyenv -> exp -> int -> tyenv * ty * tysubst * int *)
let rec tinf2 te e n =
  match e with
    | Var(s) -> 
      (try
        let t1 = lookup s te in (te, t1, theta0, n)
      with Failure(_) ->
        let (tx,n1) = new_typevar n in
        let te1 = ext te s tx in
          (te1, tx, theta0, n1))
    | IntLit(_)   -> (te, TInt, theta0, n)
    | BoolLit(_)  -> (te, TBool, theta0, n)
    | Plus(e1,e2) -> 
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,TInt); (t2,TInt)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 
                    (compose_subst theta2 theta1) in
        (te3, TInt, theta4, n2)
    | If(e1,e2,e3) ->  
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let (te3, t3, theta3, n3) = tinf2 te2 e3 n2 in    
      let t11 = subst_ty theta2 t1 in
      let t12 = subst_ty theta3 t11 in  
      let theta4 = unify [(t12,TBool)] in
      let t21 = subst_ty theta4 t2 in
      let t22 = subst_ty theta3 t21 in
      let t31 = subst_ty theta4 t3 in
      let theta5 = unify [(t22,t31)] in
      let theta6 = compose_subst theta5 (compose_subst theta4 (compose_subst theta3 (compose_subst theta2 theta1))) in
      let te4 = subst_tyenv theta6 te3 in
      (te4, t22, theta6, n3)
    | Fun(x,e) -> 
      let (tx,n1) = new_typevar n in
      let te1 = ext te x tx in
      let (te2, t1, theta1, n2) = tinf2 te1 e n1 in
      let t2 = subst_ty theta1 tx in
      let te3 = remove te2 x in
        (te3, TArrow(t2, t1), theta1, n2)
    | App(e1,e2) -> 
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let (tx,n3) = new_typevar n2 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,TArrow(t2,tx))] in
      let t3 = subst_ty theta3 tx in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 
                    (compose_subst theta2 theta1) in
        (te3, t3, theta4, n3)
    | Let(x,e1,e2) ->
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let te2 = (ext (te1) (x) t1) in
      tinf2 te2 e2 n1
    | _ -> failwith "unknown expression"

let tinf2top e = tinf2 [] e 0
let exp1 = (Let(("x"), IntLit(2), If(BoolLit(true), Var("y"), Var("x"))))