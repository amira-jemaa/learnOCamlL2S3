(* GRADE:  100% *)


(* Q1 *)
let rec hauteur (bt:'a btree) : int =
  match bt with
    Empty -> 0
  | Node (i, g, d) -> 1 + max (hauteur g) (hauteur d)

(* Q2 *)
let rec list_by_depth (bt:'a btree) (n:int) : 'a list =
  match bt with
    Empty -> []
  | Node (i, g , d) -> if n=0 then [i] 
      else list_by_depth g (n-1) @ list_by_depth d (n-1)
  
(* Q3 *)
let rec to_list (bt:'a btree) : 'a list =
  match bt with
    Empty -> []
  | Node (i, g , d) -> i :: to_list g @ to_list d
