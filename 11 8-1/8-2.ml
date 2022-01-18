

type cam_instr =    
  | CAM_Ldi of int                    (* CAM_Ldi(n) は、整数 n をスタックに積む (loadする) *)
  | CAM_Ldb of bool                   (* CAM_Ldb(b) は、真理値 b をスタックに積む (loadする) *)
  | CAM_Access of int                 (* CAM_Access(i) は、環境の i+1 番目の値をスタックに積む *)
  | CAM_Closure of cam_code           (* CAM_Closure(c) は、関数本体のコードが c で、
			               * その環境が、現在の環境であるような関数
			               * クロージャを生成し、それをスタックに積む。
			               * 前項で説明したように変数は名前の代わりに
			               * 環境のインデックスで参照されるので、
			               * このクロージャにも関数引数は含まれない。
			               * なお、この関数クロージャは、再帰関数で
			               * あるとして処理される。
			             *)
  | CAM_Apply                         (* スタックトップの値が関数クロージャならば、
			               * その関数を、スタックの上から2番めにある値に
			               * 関数適用した計算を行なう。
			               *)
  | CAM_Return                        (* 関数の呼び出し元に戻る *)
  | CAM_Let                           (* スタックトップの値を環境の先頭に移す (環境を拡張する) *)
  | CAM_EndLet                        (* 環境の先頭の値を取り除く *)
  | CAM_Test of cam_code * cam_code   (* I_Test(c1,c2)は、スタックトップの値が
			               * true ならば、コードc1 を実行し、false
			               * ならばコード c2 を実行する。
			               *)
  | CAM_Add                           (* スタックトップの値とスタックの上から2番めの値を
			               * 取り出し、その和をスタックに積む
			               *)
  | CAM_Eq                            (* スタックトップの値とスタックの上から2番めの値を
			               * 取り出し、それらが同じ整数であるかどうかテストし、
			               * その結果の真理値をスタックに積む
			               *)
  | CAM_Sub (* calculate <top of a stack> - <2nd of a stack>:int and push it *)
  | CAM_Mul (* calculate <top of a stack> * <2nd of a stack>:int and push it *)
  | CAM_Gt  (* calculate <top of a stack> > <2nd of a stack>:bool and push it *)
and cam_code = cam_instr list  (* コードは、命令の列である *)

type cam_value =  
  | CAM_IntVal  of int   (* CAM の値に対するタグにはCAM_ をつける *)
  | CAM_BoolVal of bool
  | CAM_ClosVal of cam_code * cam_env  (* 再帰関数に対応するクロージャ *)
and cam_stack = cam_value list (* スタック *)
and cam_env = cam_value list (* 環境は、1つのスタックフレームに相当する。 *)


(* let rec trans c env s =
  match c with
  | [] -> List.hd s
  | CAM_Ldi(n) :: c1 -> 
    let s1 = CAM_IntVal(n) in trans c1 env s1::s
  | CAM_Ldb(b) :: c1 -> trans c1 env CAM_BoolVal(b)::s
  | CAM_Access(i) :: c1 ->
    let s1 = List.hd env in trans c1 env s1::s
  | CAM_Closure(cc) :: c1 ->
    let s1 = ClosVal(cc,env) in trans c1 env s1::s
  | CAM_Apply :: c1 ->
    match s with
    | CAM_ClosVal(cc,envv) :: v :: s1 ->
      let env1 = v :: CAM_ClosVal(cc,envv) :: envv in
      let s2 = CAM_ClosVal(c,env) in
      trans c1 env1 s2::s
    | _ -> failwith "failed with CAM_Apply"
  | CAM_Return :: c1 ->
    match s with
    | v :: CAM_ClosVal(cc,envv) :: s1 ->
      trans c1 envv v::s1
    | _ -> failwith "failed with CAM_Return"
  | CAM_Let :: c1 ->
  | CAM_EndLet :: c1 ->
  | CAM_Test(cc1,cc2) :: c1 ->
  | CAM_Add :: c1 ->
  | CAM_Eq :: c1 ->
  | _ -> failwith "failed with trans-pattern-matching" *)

(* trans: cam_code -> cam_env -> cam_stack -> cam_value *)
  let rec trans c env s =
    match (c,env,s) with
    | ([],_,_) -> 
      if env = [] then List.hd s
      else failwith "failed cuz env isnt empty"
    | (CAM_Ldi(n)::c1,_,_) -> 
      let s1 = CAM_IntVal(n)::s in trans c1 env s1
    | (CAM_Ldb(b)::c1,_,_) -> 
      let s1 = CAM_BoolVal(b)::s in trans c1 env s1
    | (CAM_Access(i)::c1,_,_) ->
      let s1 = (List.nth env i)::s in trans c1 env s1
    | (CAM_Closure(cc)::c1,_,_) ->
      let s1 = CAM_ClosVal(cc,env)::s in trans c1 env s1
    | (CAM_Apply::c1,_,CAM_ClosVal(cc,envv)::v::s1) ->
      let env1 = v :: CAM_ClosVal(cc,envv) :: envv in
      let s2 = CAM_ClosVal(c1,env)::s1 in
      trans cc env1 s2
    | (CAM_Return::c1,_, v::CAM_ClosVal(cc,envv)::s1) ->
      let s2 = v::s1 in
      trans cc envv s2
    | (CAM_Let::c1,_,v::s1) ->
      let env1 = v::env in
      trans c1 env1 s1
    | (CAM_EndLet::c1,v::env1,_) ->
      trans c1 env1 s
    | (CAM_Test(cc1,cc2)::c1,_,b::s1) ->
      begin
        match b with
        | (CAM_BoolVal(true)) -> let ccc = cc1@c1 in trans ccc env s1
        | (CAM_BoolVal(false)) -> let ccc = cc2@c1 in trans ccc env s1
        (* | (CAM_ClosVal(c2,env2)) -> let b2 = trans c2 env2 s1 in
          if b2 = CAM_BoolVal(true) then let ccc = cc1@c1 in trans ccc env s1
          else if b2 = CAM_BoolVal(false) then let ccc = cc2@c1 in trans ccc env s1
          else failwith "failed with test" *)
        | _ -> failwith "failed with test"
      end
    | (CAM_Add::c1,_,n1::n2::s1) ->
      (* let n1' = trans n1 env s1 in
      let n2' = trans n2 env s1 in *)
      begin
        match (n1,n2) with
        | (CAM_IntVal(nn1),CAM_IntVal(nn2)) -> let s2 = CAM_IntVal(nn1+nn2)::s1 in trans c1 env s2
        | (_,_) -> failwith "failed with add"
      end
    | (CAM_Sub::c1,_,n1::n2::s1) ->
      (* let n1' = trans n1 env s1 in
      let n2' = trans n2 env s1 in *)
      begin
        match (n1,n2) with
        | (CAM_IntVal(nn1),CAM_IntVal(nn2)) -> let s2 = CAM_IntVal(nn1-nn2)::s1 in trans c1 env s2
        | (_,_) -> failwith "failed with sub"
      end
    | (CAM_Mul::c1,_,n1::n2::s1) ->
      (* let n1' = trans n1 env s1 in
      let n2' = trans n2 env s1 in *)
      begin
        match (n1,n2) with
        | (CAM_IntVal(nn1),CAM_IntVal(nn2)) -> let s2 = CAM_IntVal(nn1*nn2)::s1 in trans c1 env s2
        | (_,_) -> failwith "failed with mul"
      end
    | (CAM_Eq::c1,_,n1::n2::s1) ->
      (* let n1' = trans n1 env s1 in
      let n2' = trans n2 env s1 in *)
      begin
        match (n1,n2) with
        | (CAM_IntVal(nn1),CAM_IntVal(nn2)) -> 
        if nn1 = nn2 then let s2 = CAM_BoolVal(true)::s1 in trans c1 env s2
        else let s2 = CAM_BoolVal(false)::s1 in trans c1 env s2
        | (_,_) -> failwith "failed with eq"
      end
    | (CAM_Gt::c1,_,n1::n2::s1) ->
      (* let n1' = trans n1 env s1 in
      let n2' = trans n2 env s1 in *)
      begin
        match (n1,n2) with
        | (CAM_IntVal(nn1),CAM_IntVal(nn2)) -> 
        if nn1 > nn2 then let s2 = CAM_BoolVal(true)::s1 in trans c1 env s2
        else let s2 = CAM_BoolVal(false)::s1 in trans c1 env s2
        | (_,_) -> failwith "failed with gt"
      end
    
    | _ -> failwith "failed with trans-pattern-matching"

    let test2 =  [CAM_Ldi(4); CAM_Ldi(3); CAM_Ldi(2); CAM_Ldi(1); 
    CAM_Add; CAM_Add; CAM_Add]

    let test3 = [CAM_Closure [CAM_Ldi(1); CAM_Access 0; CAM_Add; CAM_Return];
    CAM_Let; CAM_Ldi(10); CAM_Access 0; CAM_Apply; CAM_EndLet]

    let test4 = [CAM_Ldi(2); CAM_Ldi(2); CAM_Eq;
                 CAM_Test ([CAM_Ldi(1)],[CAM_Ldi(-1)])
    ]

    let test_sub = [CAM_Ldi(2); CAM_Ldi(5); CAM_Sub;]
    let test_mul = [CAM_Ldi(2); CAM_Ldi(5); CAM_Mul;]
    let test_gt = [CAM_Ldi(2); CAM_Ldi(5); CAM_Gt;]

    let test5 = [CAM_Closure
    [CAM_Ldi 1; CAM_Access 0; CAM_Eq;
     CAM_Test ([CAM_Ldi 1],
               [CAM_Ldi (-1); CAM_Access 0; CAM_Add; CAM_Access 1;
                CAM_Apply; CAM_Access 0; CAM_Add]);
     CAM_Return];
   CAM_Let; CAM_Ldi 10; CAM_Access 0; CAM_Apply; CAM_EndLet]

    let transtop t = trans t [] []