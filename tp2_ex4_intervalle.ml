(* GRADE:  100% *)
(* Q1 *)
let rec sum_inter (a:int) (b:int) : int = 
  if(a<=b) then if(a=b) then a
    else a+(sum_inter (a+1) b)
  else 0

(* Q2 *)
let rec sum1_inter (k:int) (a:int) (b:int) : int =
  if(a<=b) then if(a=b) then a+k
    else (a+k)+(sum1_inter k (a+1) b)
  else 0

(* Q3 *)
let sum2_inter (a:int) (b:int) : int = 
  match a<=b with
    true -> (b-a+1)*((a+b)*(b-a+1))
  |false->0
  
