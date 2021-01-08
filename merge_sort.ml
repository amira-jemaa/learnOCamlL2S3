(* GRADE:  100% *)
(* Q1 *)
let rec merge (xs:'a list) (ys:'a list) : 'a list =
  match xs, ys with
  | [], _ -> ys
  | _, [] -> xs
  | hx :: txs, hy :: tys ->
      if hx < hy then hx :: merge txs ys else hy :: merge xs tys

(* Q2 *)
let rec split (xs:'a list) : ('a list * 'a list) =
  match xs with
  | [] -> ([],[])
  | x :: [] -> (x :: [],[])
  | x :: y :: xs -> let (l1,l2) = split xs in (x :: l1, y :: l2)

(* Q3 *)
let rec merge_sort (xs:'a list) : 'a list =
  match xs with
  | [] -> xs
  | x :: [] -> x :: [] 
  | _ -> let (a,b) = (split xs) in (merge (merge_sort a) (merge_sort b))

(* Q4 *)
let rec merge_gen (cmp:'a -> 'a -> bool) (xs:'a list) (ys:'a list) : 'a list =
  match xs with
  | [] -> ys
  | x :: xs -> match ys with
    | [] -> x :: xs
    | y :: ys -> if cmp x y
        then x :: merge_gen cmp xs (y :: ys)
        else y :: merge_gen cmp (x :: xs) ys
  
(* Q5 *)
let rec merge_sort_gen (cmp:'a -> 'a -> bool) (xs:'a list) : 'a list =
  match xs with 
  | [] -> []
  | [x] -> [x] 
  | _ -> let (l1,l2) = split xs in 
      ( merge_gen (cmp) (merge_sort_gen cmp l1) (merge_sort_gen cmp l2) ) 

(* Q6 *)
let sort (xs:(int*int) list) : (int*int) list = 
  match xs with
  | [] -> []
  | x :: [] -> x :: []
  | _ -> merge_sort_gen (fun (x,y) (x1,y1) -> (x+y)<(x1+y1) ) xs
    


