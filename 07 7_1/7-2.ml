type exp =
  | IntLit of int
  | BoolLit of bool
  | Plus of exp * exp 
  | If of exp * exp * exp 
  | Eq of exp * exp

type ty = TInt | TBool

(* tcheck1 : exp -> ty *)
let rec tcheck1 e =
  match e with
  | IntLit(_)    -> TInt
  | BoolLit(_)   -> TBool
  | Plus(e1,e2)  -> 
      begin
        match (tcheck1 e1, tcheck1 e2) with
          (TInt,TInt) -> TInt
        | _ -> failwith "type error in Plus"
      end
  | If(e1,e2,e3) -> 
      begin
        match (tcheck1 e1, tcheck1 e2, tcheck1 e3) with
          (TBool,TInt,TInt) -> TInt
        | (TBool,TBool,TBool) -> TBool
        | _ -> failwith "type error in IF"
      end
  | Eq(e1,e2) ->
      begin
        match (tcheck1 e1, tcheck1 e2) with
        | (TBool,TBool) -> TBool
        | (TInt,TInt) -> TBool
        | _ -> failwith "type error in EQ"
      end
  | _ -> failwith "unknown expression"