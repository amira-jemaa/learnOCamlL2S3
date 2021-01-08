(* GRADE:  100% *)
(* Q1 *)
let rec u (n:int) : int =
  if (n>=0) then if(n=0) then 42
    else 3*(u (n-1))+4
  else failwith "indéfinie"

(* Q2 *)
let rec sum_u (n:int) : int =
  if(n=0) then u 0
  else (u n)+sum_u (n-1)
         
(* Q3 *)
let sum_u2 (n:int) : int =
  let rec loop (n:int) (t:int) : int = 
    if (n=0) then t
    else t+(loop (n-1) (3*t+4))
  in loop n 42

