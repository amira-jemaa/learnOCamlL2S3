(* GRADE:  100% *)
(* Q1 *)
let rec lt_btree (bt:'a btree) (x:'a) : bool =
  match bt with
    Empty -> true
  | Node (n, g, d) -> if n < x then
        if lt_btree g x then
          if lt_btree d x then true
          else false
        else false
      else false

(* Q2 *)
let rec ge_btree (bt:'a btree) (x:'a) : bool =
  match bt with
    Empty -> true
  | Node (n, g, d) -> if n >= x then
        if ge_btree g x then if ge_btree d x then true
          else false
        else false
      else false

(* Q3 *)
let rec is_abr (bt:'a btree) : bool =
  match bt with
    Empty -> true
  | Node (n, g, d) -> if lt_btree g n && ge_btree d n then true
      else false
  
(* Q4 *)
let rec mem (bt:'a btree) (x:'a) : bool =
  match bt with
    Empty -> false
  | Node (n, g, d) -> if n=x then true 
      else if x<n then mem g x 
      else mem d x
