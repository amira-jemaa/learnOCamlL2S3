(* GRADE:  100% *)
(* Q1 *)
let f (a:float) (x:float) : float =
  if (x>0.0) then (1.0/.2.0)*.(x+.(a/.x))
  else failwith "indéfinie"

(* Q2 *)
let rec sqrt_n (n:int) (a:float) (x0:float) =
  if (n>=0) then if (n=0) then f a x0
    else sqrt_n (n-1) a (f a x0)
  else failwith "indéfinie"

(* Q3 *)
let eq_eps (e:float) (x:float) (y:float) : bool =
  if(e>0.0 && x>=0.0 && y>=0.0) then if(((x>y) && (x-.y)<e) || ((x<y) && (y-.x)<e)) then true
    else if (x=y) then true
    else false
  else failwith "indéfinie"

(* Q4 *)
let sqrt_x (e:float) (a:float) (x0:float) : float =
  let rec boucle (x:float) : float=
    if(eq_eps e x (f a x)) then f a x
    else boucle (f a x)
  in boucle x0    
