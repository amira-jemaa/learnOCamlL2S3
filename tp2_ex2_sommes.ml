(* GRADE:  100% *)
(* Q1 *)
let rec sum_n (n:int) : int =
  if (n<0) then failwith "n n'est pas strictement positif."
  else if (n=0) then 0
  else n + sum_n (n-1)

(* Q2 *)
let sum_n2 (n:int) : int =
  if (n<0) then raise (Invalid_argument "sum_n")
  else sum_n n

(* Q3 *)
let rec sum_p (n:int) : int =
  if (n=0) then 0
  else (n*2)+(sum_p (n-1))

(* Q4 *)
let rec sum_f (f : int -> int) (n:int) : int =
  if (n=0) then f n
  else (f n)+(sum_f f (n-1))

let sum_p2 (n:int) : int = 
  let f x = 2*x in sum_f f n

