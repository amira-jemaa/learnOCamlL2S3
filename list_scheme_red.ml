(* GRADE:  100% *)

let rec prod (xs:float list) : float =
  match xs with
    []->1.0
  |x1::x->x1*.prod x
  
let rec sum_round (xs:float list) : int =
  match xs with
    []->0
  |x1::x->int_of_float(x1)+sum_round x
  
let rec parenthese (xs:string list) : string =
  match xs with
    []->""
  |x1::x->"("^x1^")"^parenthese x 
            
let rec flatten (xss:('a list) list) : 'a list =
  match xss with
    []->[]
  |x1::x->match x1 with
      []->flatten x
    |x2::xs->x2::flatten (xs::x)
  
let rec sum_tuple (cs:(int*int) list) : int =
  match cs with
    []->0
  |(a,b)::x->a+b+sum_tuple x

let rec reduce (f:'a -> 'b -> 'b) (xs:'a list) (b:'b) : 'b =
  match xs with
    []->b
  |x1::x->f x1 (reduce f x b)
  


