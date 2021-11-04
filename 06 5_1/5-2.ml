(* 式の型 *)
type exp =
  |  IntLit of int
  |  Plus of exp * exp 
  |  Times of exp * exp
  |  BoolLit of bool        
  |  If of exp * exp * exp  
  |  Eq of exp * exp        
  |  Greater of exp * exp
  |  Var of string        
  |  Let of string * exp * exp 
  |  Fun of string * exp
  |  App of exp * exp

(* 値の型 *)
type value =
  | IntVal  of int          
  | BoolVal of bool     
  | FunVal of string * exp * ((string * value) list)    

let emptyenv () = []
let ext env x v = (x,v) :: env

let rec lookup x env =
    match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y,v)::tl -> if x=y then v 
      else lookup x tl

let rec eval4 e env =         
let binop f e1 e2 env =       
   match (eval4 e2 env, eval4 e1 env) with
   | (IntVal(n2),IntVal(n1)) -> IntVal(f n1 n2)
   | _ -> failwith "integer value expected"
in 
match e with
| Var(x)       -> lookup x env 
| IntLit(n)    -> IntVal(n)
| Plus(e1,e2)  -> binop (+) e1 e2 env     
| Times(e1,e2) -> binop ( * ) e1 e2 env   
| Eq(e1,e2)    -> 
   begin
      match (eval4 e1 env, eval4 e2 env) with
      | (IntVal(n1),IntVal(n2)) -> BoolVal(n1=n2)
      | (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1=b2)
      | _ -> failwith "wrong value"
   end
| If(e1,e2,e3) ->
   begin
      match (eval4 e1 env) with          
      | BoolVal(true)  -> eval4 e2 env  
      | BoolVal(false) -> eval4 e3 env  
      | _ -> failwith "wrong value"
   end
| Let(x,e1,e2) -> 
   let env1 = ext env x (eval4 e1 env) 
   in eval4 e2 env1
| Fun(x,e1) -> FunVal(x, e1, env)
| App(e1,e2) ->
let funpart = (eval6 e1 env) in
let arg = (eval6 e2 env) in
  begin
   match funpart with
   | FunVal(x,body,env1) ->
      let env2 = (ext env1 x arg) in
      eval6 body env2
   | RecFunVal(f,x,body,env1) ->
      let env2 = (ext (ext env1 x arg) f funpart) in
      eval6 body env2
   | _ -> failwith "wrong value in App"
  end

| _ -> failwith "unknown expression"