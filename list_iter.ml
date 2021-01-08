(* GRADE:  100% *)

(* Q1 *)
let inverse_i (xs:int list) : float list =
  List.map (fun x -> if x!=0 then 1. /. float_of_int x
             else 0. ) xs

(* Q2 *)
let list_sum_tuple (cs:(int*int) list) (s:int) : (int*int) list =
  List.filter (fun x -> let(a, b) = x in (a+b) >= s ) cs

(* Q3 *)
let parenthese (xs:string list) : string =
  if xs = [] then ""
  else List.fold_right (fun x acc -> "("^x^")"^acc) xs ""

(* Q4 *)
let list_non_0 (nss:(int list) list) : (int list) list =
  List.filter(fun x -> not(List.mem 0 x)) nss

(* Q5 *)
let dpoints (xs:int list) (a:int) (b:int) : (int*int) list =
  List.map (fun x -> (x, x*a+b)) xs

(* Q6 *)
let prod_sum_tuple (cs:(int*int) list) : int =
  List.fold_left (fun acc x -> let (a, b)=x in (a+b)*acc ) 1 cs
