(* GRADE:  100% *)

let rec list_impair (ns:int list) : int list =
  match ns with
    []->[]
  |x1::x->match (x1 mod 2)!=0 with
      true->x1::list_impair x
    |false->list_impair x
    
let rec list_non_nulle (xs:string list) : string list =
  match xs with
    []->[]
  |x1::x->match x1 with
      ""->list_non_nulle x 
    |_->x1::list_non_nulle x
  
let rec list_interval (ns:int list) : int list =
  match ns with
    []->[]
  |x1::x->match x1>10 with
      true->list_interval x
    |_->match x1<(-10) with
        true->list_interval x
      |false->x1::list_interval x

let rec list_non_vide (xss:('a list) list) : ('a list) list =
  match xss with
    []->[]
  |x1::x->match x1 with
      []->list_non_vide x
    |_->x1::list_non_vide x

let rec list_non_0 (nss:(int list) list) : ('int list) list =
  match nss with
    []->[]
  |x1::x->match List.mem 0 x1 with
      true->list_non_0 x
    |_->x1::list_non_0 x

let rec list_sum_tuple (cs:(int * int) list) (s:int) : (int * int) list =
  match cs with
    []->[]
  |x1::x->match x1 with
      (a,b)->match (a+b)>=s with
        true->x1::list_sum_tuple x s
      |_->list_sum_tuple x s

