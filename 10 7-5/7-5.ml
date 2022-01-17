type exp =
  | Var of string
  | IntLit of int
  | BoolLit of bool
  | Plus of exp * exp 
  | If of exp * exp * exp 

type ty = TInt | TBool | TVar of string
type tyenv = (string * ty) list

let ext env x v = (x,v) :: env

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y,v)::tl -> if x=y then v 
    else lookup x tl

(* new_typevar : string -> ty *)
let new_typevar s = TVar("'" ^ s)

(* substitute : ty -> ty -> tyenv -> tyenv *)
let rec substitute2 tvar t te =
  match te with
  | [] -> []
  | (x,t2) :: te2 -> 
      let t3 = (if t2 = tvar then t else t2) in
      (x,t3) :: (substitute2 tvar t te2)
  
let rec substitute tvar t te =
  List.map (fun (x,t2) ->
    if t2 = tvar then (x,t) else (x,t2))
  te

(* tinf1 : tyenv -> exp -> tyenv * ty *)
let rec tinf1 te e =
  match e with
  | IntLit(_)    -> (te, TInt)
  | BoolLit(_)  -> (te, TBool)
  | Var(s)    ->  
        (try 
          let t1 = lookup s te in
            (te, t1)
	 with Failure(_) ->   (* 変数s が型環境 te に含まれなかった時 *)
            let tvar = new_typevar s in
            let te1 = ext te s tvar in
               (te1, tvar))
  | Plus(e1,e2)  -> 
       let (te1, t1) = tinf1 te e1 in
       let te2 = 
         begin
          match t1 with
          | TInt -> te1
          | TVar(s) -> substitute t1 TInt te1
          | _ -> failwith "type error in Plus"
         end
       in 
       let (te3, t2) = tinf1 te2 e2 in
       let te4 = 
         begin
          match t2 with
          | TInt -> te3
          | TVar(s) -> substitute t2 TInt te3
          | _ -> failwith "type error in Plus"
         end
       in 
         (te4, TInt)
  | If(e1,e2,e3) -> 
       let (te1, t1) = tinf1 te e1 in
       let te2 = 
         begin
          match t1 with
          | TBool -> te1
          | TVar(s) -> substitute t1 TBool te1
          | _ -> failwith "type error in IF"
         end
       in 
       let (te3, t2) = tinf1 te2 e2 in
       let (te4, t3) = tinf1 te3 e3 in
         begin
          match (t2,t3) with
          | (TInt,TInt) -> (te4, TInt)
          | (TBool,TBool) -> (te4, TBool)
          | (TInt,TVar(_)) -> 
	      let te5 = substitute t3 TInt te4 in (te5, TInt)
          | (TVar(_),TInt) -> 
	      let te5 = substitute t2 TInt te4 in (te5, TInt)
          | (TBool,TVar(_)) ->
	      let te5 = substitute t3 TBool te4 in (te5, TBool)
          | (TVar(_),TBool) -> 
	      let te5 = substitute t2 TBool te4 in (te5, TBool)
          | (TVar(_),TVar(_)) -> 
	      let te5 = substitute t2 t3 te4 in (te5, t3)
          | _ -> failwith "type error in IF"
         end

let exp = (If(Var("x"), IntLit(2), IntLit(3)))
let exp2 = (If(Var("x"), Var("y"), IntLit(3)))
let exp3 = (If(Var("x"), Var("y"), Var("y")))
let exp4 = (If(Var("x"), Var("x"), Var("y")))
let exp5 = (If(Var("x"), Var("x"), IntLit(3)))
let exp6 = (Plus(Var("x"), IntLit(3)))
let exp7 = (Plus(Var("x"), BoolLit(false)))

let rec longenv a b =
  if a > b then []
  else (string_of_int(a),TVar("'" ^ string_of_int(a))) :: longenv (a+1) b;;

let thousandtenv = longenv 1 1

let rec thousandsub te x =
  if x < 0 then te
  else let tenv = substitute2 (TVar("'20")) (TBool) (te) in
  thousandsub tenv (x-1)

let time : (unit -> 'a) -> float =
  fun f ->
    let start = Sys.time () in
    let res   = f () in
    let end_  = Sys.time () in
    end_ -. start