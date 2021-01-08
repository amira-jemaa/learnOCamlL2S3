(* GRADE:  100% *)
    
let hd_fst (xs:('a*'b) list) : 'a =
  match xs with
    []->raise Not_found
  | x1::xs->match x1 with
      (a,b)->a 
    
let swap_hd_fst (xs:('a*'a) list) : ('a*'a) list =
  match xs with
    []->[]
  |x1::xs->match x1 with
      (a,b)->(b,a)::xs
             
    
let hd_hd (xs:('a list) list) : 'a =
  match xs with
    []->raise Not_found
  |x1::xs->match x1 with
      []->raise Not_found 
    |x::x1->x
    
let rem_hd_hd (xs:('a list) list) : (int list) list =
  match xs with
    []->[]
  |x1::xs->match x1 with
      []->[]::xs
    |x::x1->x1::xs
  

