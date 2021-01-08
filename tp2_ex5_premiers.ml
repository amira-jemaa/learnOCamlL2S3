(* GRADE:  100% *)
(* Q1 *)
let rec less_divider (i:int) (n:int) : int =
  if (i<=n && i>=0 && n>=0) then if (i=n) then 0
    else if ((n mod i)=0) then i
    else less_divider (i+1) n
  else failwith "invalid argument"


(* Q2 *)
let prime (n:int) : bool =
  if(n>1) then 
    if ((less_divider 2 n)=0) then true
    else false
  else false 

(* Q3 *)
let rec next_prime (n:int) : int =
  if (n>=0) then if (prime n) then n
    else next_prime (n+1)
  else failwith "invalid argument"
      
(* Q4  *)
let nth_prime (n:int) : int = 
  if (n>=0) then let rec boucle (i:int) (c:int) : int =
                   if (c=0) then i
                   else boucle (next_prime (i+1)) (c-1)
    in boucle 2 n
  else failwith "invalid argument"

