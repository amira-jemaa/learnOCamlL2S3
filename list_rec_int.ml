(* GRADE:  100% *)
(* Q1 *)
let  repeat (n:int) (x:'a) : 'a list  = 
  if(n<0) then []
  else 
    let rec boucle n2 x1 =
      match n2 with 
        0->[] 
      |_->x1::(boucle (n2-1) x1)
    in boucle n x


(* Q2 *)
let range_i (i:int) (j:int) : (int list) =
  if(i>j) then []
  else
    let rec boucle x y =
      match x>y with
        true->[]
      |_->x::(boucle (x+1) y)
    in boucle i j

(* Q3 *)
let range_n (x:int) (n:int) : (int list) =
  if(n<=0) then []
  else 
    let rec boucle y i =
      match y>=(x+i) with
        true->[]
      |_->y::(boucle (y+1) i)
    in boucle x n
    







