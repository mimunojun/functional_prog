let rec gcd = fun x y ->
  if x = y then
    x
  else if x > y then
    gcd (x-y) x
  else
    gcd x (y-x)
;;


let rec fib_ = fun x a b ->
  if x = 0 then
    a
  else
    fib_ (x-1) b (a+b)

let fib x = fib_ x 0 1


let rec prime_check = fun x b ->
  if (x=b) || (x<=2) then
    true
  else if (x mod b = 0) then
    false
  else
    prime_check x (b+1);;

let rec prime_ = fun n i ->
  if n = 0 then
    i-1
  else
    if (prime_check i 2) = false then
      prime_ n (i+1)
    else
      prime_ (n-1) (i+1);;

let prime n = prime_ n 2



let rec substring_ = fun s1 s2 len i j->
  if len = 0 then
    i - (String.length s2)
  else if (i = String.length s1) then
    -1
  else if ((s1.[i]) = (s2.[j])) then
    substring_ s1 s2 (len-1) (i+1) (j+1)
  else
    substring_ s1 s2 (String.length s2) (i+1) j

let substring s1 s2 = substring_ s1 s2 (String.length s2) 0 0


let rec length lst =
  match lst with
    [] ->   0
  | x::l ->  (length l) + 1 ;;



let rec quicksort lst=
  match lst with
    [] -> []     (* base case *)
  | hd::tl ->
      let (lt, gt) = List.partition (fun i -> i < hd) tl in
        (quicksort lt)@[hd]@(quicksort gt);;
