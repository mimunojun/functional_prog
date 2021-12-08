type exp =
  | IntLit of int
  | BoolLit of bool
  | Plus of exp * exp 
  | If of exp * exp * exp 
  | Eq of exp * exp
  | Var of string

type ty = TInt | TBool
type tyenv = (string * ty) list

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y,v)::tl -> if x=y then v 
    else lookup x tl

(* tcheck2 : tyenv -> exp -> ty *)
let rec tcheck2 te e =
  match e with
  | Var(s) -> lookup s te
  | IntLit(_)    -> TInt
  | BoolLit(_)   -> TBool
  | Plus(e1,e2)  -> 
      begin
        match (tcheck2 te e1, tcheck2 te e2) with
          (TInt,TInt) -> TInt
        | _ -> failwith "type error in Plus"
      end
  | If(e1,e2,e3) -> 
      begin
        match (tcheck2 te e1, tcheck2 te e2, tcheck2 te e3) with
          (TBool,TInt,TInt) -> TInt
        | (TBool,TBool,TBool) -> TBool
        | _ -> failwith "type error in IF"
      end
  | Eq(e1,e2) ->
      begin
        match (tcheck2 te e1, tcheck2 te e2) with
        | (TBool,TBool) -> TBool
        | (TInt,TInt) -> TBool
        | _ -> failwith "type error in EQ"
      end
  | _ -> failwith "unknown expression"

  let e1 = If(BoolLit(true), Var("x"), IntLit(100));;
