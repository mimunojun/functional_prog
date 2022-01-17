type exp =
  | Var of string
  | IntLit of int
  | BoolLit of bool
  | Plus of exp * exp 
  | If of exp * exp * exp 

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

(* new_typevar : string -> ty *)
let new_typevar s = TVar("'" ^ s)

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

 let tyenv1 = [(TVar("'a"),TBool)]
 let tyenv2 = [(TInt,TBool)]
 let tyenv3 = [(TVar("'a"),TVar("'b"))]
 let tyenv4 = [(TArrow(TVar("'a"),TVar("'b")), TArrow(TVar("'b"), TVar("'c")))]
 let tyenv5 = [(TVar("'a"),TArrow(TVar("'b"),TVar("'a")))]