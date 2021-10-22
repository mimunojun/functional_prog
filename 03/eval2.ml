(* 式の型 *)
type exp =
  |  IntLit of int
  |  Plus of exp * exp 
  |  Times of exp * exp
  |  BoolLit of bool        (* 追加分; 真理値リテラル, つまり trueや false  *)
  |  If of exp * exp * exp  (* 追加分; if-then-else式 *)
  |  Eq of exp * exp        (* 追加分; e1 = e2 *)
  |  Greater of exp * exp

(* 値の型 *)
type value =
  | IntVal  of int          (* 整数の値 *)
  | BoolVal of bool         (* 真理値の値 *)

(* eval2 : exp -> value *)
let rec eval2 e =
  match e with
  | IntLit(n)  -> IntVal(n)
  | Plus(e1,e2) ->
      begin
        match (eval2 e1, eval2 e2) with
        | (IntVal(n1),IntVal(n2)) -> IntVal(n1+n2)
        |_ -> failwith "integer values expected"
      end
  | Times(e1,e2) ->
      begin
        match (eval2 e1, eval2 e2) with
        | (IntVal(n1),IntVal(n2)) -> IntVal(n1*n2)
        |_ -> failwith "integer values expected"
      end
  | Eq(e1,e2) ->
    begin
      match (eval2 e1, eval2 e2) with
        | (IntVal(n1),IntVal(n2)) -> BoolVal(n1=n2)
        | (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1=b2)
        | _ -> failwith "wrong value"
    end
  | BoolLit(b) -> BoolVal(b)
  | If(e1,e2,e3) ->
      begin
        match (eval2 e1) with
          | BoolVal(true) -> eval2 e2
          | BoolVal(false) -> eval2 e3
          | _ -> failwith "wrong value"
      end
  | Greater(e1, e2) ->
    begin
      match (eval2 e1, eval2 e2) with
        | (IntVal(n1), IntVal(n2)) -> BoolVal(n1>n2)
        | _ -> failwith "wrong value"
    end
  | _ -> failwith "unknown expression"

(* eval2b : exp -> value *)
let rec eval2b e =
  let binop f e1 e2 =
    match (eval2b e1, eval2b e2) with
    | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer values expected"
  in 
  let binop_bool f e1 e2 =
    match (eval2b e1, eval2b e2) with
    | (IntVal(n1),IntVal(n2)) -> BoolVal(f n1 n2)
    | _ -> failwith "integer values expected"
  in
  match e with
  | IntLit(n)    -> IntVal(n)
  | BoolLit(b)   -> BoolVal(b)
  | Plus(e1,e2)  -> binop (+) e1 e2
  | Times(e1,e2) -> binop ( * ) e1 e2
  | Greater(e1,e2) -> binop_bool (>) e1 e2
  | Eq(e1,e2) ->
    begin
      match (eval2 e1, eval2 e2) with
        | (IntVal(n1),IntVal(n2)) -> BoolVal(n1=n2)
        | (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1=b2)
        | _ -> failwith "wrong value"
    end
  | If(e1,e2,e3) ->
    begin
      match (eval2 e1) with
        | BoolVal(true) -> eval2 e2
        | BoolVal(false) -> eval2 e3
        | _ -> failwith "wrong value"
    end
  | _ -> failwith "unknown expression"

  eval3 (Var("x")) (envi) ;;
  let env = ext env "y" (IntVal 6);;
  eval3
  (If(Eq(Var("x"),Plus(IntLit(1), IntLit(2))), IntLit(1), IntLit(0)))
  (env)
  ;;

  eval3
  (Let("x", IntLit(1), Let("x", IntLit(2), Var("x"))))
  (env)
  ;;

  eval3
  (Let("x", IntLit(1), Let("y", Plus(Var("x"), IntLit(1)), Plus(Var("x"), Var("y")))))
  (env)
  ;;

  eval3
  (Let("x", IntLit(1), Plus(Let("x", IntLit(2), Plus(Var("x"), IntLit(1))), Times(Var("x"), IntLit(2)))))
  (env)
  ;;

  let env = emptyenv();;
  let env = ext env "x" (IntVal 3);;