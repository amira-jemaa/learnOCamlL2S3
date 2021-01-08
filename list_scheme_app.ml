(* GRADE:  100% *)

let rec inverse_f (xs:float list) : float list =
  match xs with
    []->[]
  |x1::x->1.0/.x1::inverse_f x 

let rec inverse_i (ns:int list) : float list =
  match ns with
    []->[]
  |x1::x->float_of_int(1)/.float_of_int(x1)::inverse_i x 

let rec ecrete (xs:int list) : int list =
  match xs with
    []->[] 
  |x1::x->match (x1>10) with
      true->10::ecrete x
    |_->match x1<(-10) with
        true->(-10)::ecrete x
      |_->x1::ecrete x
      
      
      

let rec dpoints (xs:int list) (a:int) (b:int) : (int*int) list =
  match xs with
    []->[]
  |x1::x->(x1,a*x1+b)::dpoints x a b

let rec app_list (f:'a -> 'b) (xs:'a list) : 'b list =
  match xs with
    []->[]
  |x1::x->f x1::app_list f x

