(* 式の型 *)
type exp =
   |  IntLit of int
   |  Plus of exp * exp 
   |  Times of exp * exp
   |  BoolLit of bool        (* 追加分; 真理値リテラル, つまり trueや false  *)
   |  If of exp * exp * exp  (* 追加分; if-then-else式 *)
   |  Eq of exp * exp        (* 追加分; e1 = e2 *)
   |  Greater of exp * exp
   |  Var of string        
   |  Let of string * exp * exp 
 
 (* 値の型 *)
 type value =
   | IntVal  of int          (* 整数の値 *)
   | BoolVal of bool         (* 真理値の値 *)

let emptyenv () = []

(* let rec printenv env = 
   match env with
   | [] -> print_string "end"
   | (x,v)::tl -> 
   begin
      print_string (x ^ "," ^ (str) ^ ";");
      printenv tl
   end *)
   

let ext env x v = (x,v) :: env

let rec lookup x env =
   match env with
   | [] -> failwith ("unbound variable: " ^ x)
   | (y,v)::tl -> if x=y then v 
      else lookup x tl

let rec eval3 e env =           (* env を引数に追加 *)
let binop f e1 e2 env =       (* binop の中でも eval3 を呼ぶので env を追加 *)
   match (eval3 e1 env, eval3 e2 env) with
   | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
   | _ -> failwith "integer value expected"
in 
match e with
| Var(x)       -> lookup x env 
| IntLit(n)    -> IntVal(n)
| Plus(e1,e2)  -> binop (+) e1 e2 env     (* env を追加 *)
| Times(e1,e2) -> binop ( * ) e1 e2 env   (* env を追加 *)
| Eq(e1,e2)    -> 
   begin
      match (eval3 e1 env, eval3 e2 env) with
      | (IntVal(n1),IntVal(n2)) -> BoolVal(n1=n2)
      | (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1=b2)
      | _ -> failwith "wrong value"
   end
| If(e1,e2,e3) ->
   begin
      match (eval3 e1 env) with          (* env を追加 *)
      | BoolVal(true)  -> eval3 e2 env   (* env を追加 *)
      | BoolVal(false) -> eval3 e3 env   (* env を追加 *)
      | _ -> failwith "wrong value"
   end
| Let(x,e1,e2) -> 
   let env1 = ext env x (eval3 e1 env) 
   in eval3 e2 env1
| _ -> failwith "unknown expression"

let e = emptyenv();;

eval4
   (
      Let("x", IntLit(1), 
         Let("f", Fun("y", Plus(Var("x"),Var("y"))),
            Let("x", IntLit(2),
               App(Var("f"), Plus(Var("x"), IntLit(3)))
            )
         )
      )
   )
   (e)
;;

eval4
   (
      Let(
         "f",
         Fun(
            "y",
            If(
               Eq(Var("y"), IntLit(5)) ,
               IntLit(1),
               Plus(IntLit(1), App(Var("f"),Plus(IntLit(1), Var("y"))))
            )
         ),
         App(Var("f"),IntLit(3))
      )
   )
   (e)
;;

eval6
   (
      LetRec("f", "x", Var("x"), IntLit(0))
   )
   (emptyenv())
;;

eval6
   (
      LetRec("f", "x", Var("x"), App(Var("f"), IntLit(0)))
   )
   (emptyenv())
;;

eval6
   (
      LetRec("f", "x", If(Eq(Var("x"),IntLit(0)), IntLit(1), Plus(IntLit(2), App(Var("f"), Plus(Var("x"), IntLit(-1))))), App(Var("f"), IntLit(0)))
   )
   (emptyenv())
;;

eval6
   (
      LetRec("f", "x", If(Eq(Var("x"),IntLit(0)), IntLit(1), Times(Var("x"), App(Var("f"), Plus(Var("x"), IntLit(-1))))), App(Var("f"), IntLit(3)))
   )
   (emptyenv())
;;

eval6
   (
      LetRec("f", "x", If(Eq(Var("x"),IntLit(0)), IntLit(1), Times(Var("x"), App(Var("f"), Plus(Var("x"), IntLit(-1))))), App(Var("f"), IntLit(5)))
   )
   (emptyenv())
;;

eval6
   (
      LetRec("fib", "x",
            If(Greater(IntLit(3), Var("x")), IntLit(1), Plus(App(Var("fib"), Plus(Var("x"), IntLit(-1))), App(Var("fib"), Plus(Var("x"), IntLit(-2))))),
            App(Var("fib"), IntLit(2))
            )     
   )
   (emptyenv())
;;