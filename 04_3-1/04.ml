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
            App(Var("fib"), IntLit(7))
      )     
   )
   (emptyenv())
;;

eval6
   (
      LetRec("gcd", "x",
         
      )
   )
;;

tcheck1(If(BoolLit(true), IntLit(1), IntLit(100)))

tcheck1( If(BoolLit(true), Plus(IntLit(1), BoolLit(false)), IntLit(100)) )

tcheck1( If(BoolLit(true), Plus(IntLit(1), IntLit(2)), IntLit(100)) )

tcheck1( If(BoolLit(true), BoolLit(false),  BoolLit(true)) )

tcheck1( Eq(BoolLit(false),  IntLit(20)) )
tcheck1( Eq(BoolLit(false),  BoolLit(true)) )

(tcheck2 [("x",TInt); ("y",TInt)] e1)
(tcheck2 [("x",TBool); ("y",TInt)] e1)
(tcheck2 [("z",TInt); ("y",TInt)] e1)

(tcheck2 [("x",TInt);] (If(Var("x"), Var("x"), IntLit(100))))

(tcheck3 [("x",TInt)] (fun x -> if true then x else 100))
(tcheck3 [("x",TInt)] (Fun("x", If(BoolLit(true), Var("x"), IntLit(100)))))

(tcheck3 [("x",TInt)] (Fun("x", (If(BoolLit(true), Var("x"), IntLit(100))))))
(tcheck3 [("x",TInt)] (Fun("x", IntLit(200))))

(tcheck3 [("x",TBool)] (Fun("x", (If(BoolLit(true), Var("x"), IntLit(100))))))

((fun x -> if true then x else 100) (if true then y else 200))
(tcheck3 [("x",TInt);("y",TInt)] (App(Fun("x", If(BoolLit(true), Var("x"), IntLit(100))), (If(BoolLit(true), Var("y"), IntLit(200))))));;

(fun f -> (fun x -> f (f (f x + 10)))) 
(tcheck3 [("x",TInt); ("f",TArrow(TInt, TInt))] (Fun("f", Fun("x", App(Var("f"), App(Var("f"), App(Var("f"), Plus(Var("x"), IntLit(10)))))))));;

(tcheck3 [("x",TInt)] (Fun("x", If(BoolLit(true), Var("x"), IntLit(100)))));;

(fun f -> (fun g -> (fun x -> f (g x)))) 
(tcheck3 [("f",TArrow(TInt, TBool));("g",TArrow(TBool, TInt));("x",TBool)] (Fun("f", Fun("g", Fun("x", App(Var("f"), App(Var("g"), Var("x"))))))));;

(if true then (fun x -> x+1) else (fun y -> y*2))
(tcheck3 [("x",TInt);("y",TInt)] (If(BoolLit(true), Fun("x", Plus(Var("x"), IntLit(1))), Fun("y", Plus(Var("y"), Var("y"))))));;

fun x -> let y = x + 10 in y + 5)
(tcheck3 [("x",TInt); ("y",TInt)] (Fun("x", Let("y", Plus(Var("x"), IntLit(10)), Plus(Var("y"), IntLit(5))))));;

(tcheck3 [("x",TBool); ("y",TBool)] (Greater(Var("x"),Var("y"))));;
(tcheck3 [("x",TInt); ("y",TInt)] (Times(Var("x"),Var("y"))));;
