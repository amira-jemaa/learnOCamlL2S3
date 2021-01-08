(* GRADE:  100% *)
(* Q1 *)
let rec sublac (xs : 'a list) (ys : 'a list) : bool = 
  match xs with 
    [] -> true 
  | x :: xs -> match ys with
      [] -> false
    | y :: ys -> if x = y then sublac xs ys 
        else sublac (x :: xs) ys 

(* Q2 *)
let rec sublying (xs : 'a list)(ys : 'a list) : bool = 
  if (List.length xs != List.length ys) then false 
  else match xs with 
      [] -> if ys = [] then true else failwith"16" 
    | x :: xs -> match ys with 
        [] -> failwith"18" 
      | y :: ys -> if x != 0 && y=x then sublying xs ys 
          else if x=0 then sublying xs ys 
          else false


(* Q3 *)
let rec stretch (xs : 'a list) (ys : 'a list) : 'a list = 
  match ys with 
    [] -> if xs != [] then raise(Invalid_argument "stretch") 
      else []
  | y1 :: y2 -> match xs with 
      [] -> 0::stretch [] y2 
    | x1 :: x2 -> if x1=y1 then x1::stretch x2 y2 
        else 0 :: stretch xs y2 

