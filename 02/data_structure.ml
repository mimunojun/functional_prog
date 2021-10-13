type exp =
     IntLit of int         (* リテラル *)
  |  Plus of exp * exp     (* 足し算 *)
  |  Times of exp * exp    (* かけ算 *)
  |  Sub of exp * exp      (* 引き算 *)
  |  Div of exp * exp      (* 割り算 *)

let exp_plus_two e:exp = Plus(e, IntLit 2);;

(* eval1 : exp -> int *)
let rec eval1 e =
  match e with
  | IntLit(n) -> n
  | Plus(e1,e2) -> (eval1 e1) + (eval1 e2)
  | Times(e1,e2) -> (eval1 e1) * (eval1 e2)
  | Sub(e1, e2) -> (eval1 e1) - (eval1 e2)
  | Div(e1, IntLit 0) -> failwith "divide by zero"
  | Div(e1, e2) -> (eval1 e1) / (eval1 e2)
  | _ -> failwith "unknown expression"
