(* GRADE:  100% *)
let len_eq_3 (xs:'a list) : bool = 
  match xs with
    x1::x2::xs::[]-> true
  | _ -> false
    
let len_ge_3 (xs:'a list) : bool =
  match xs with
    x1::x2::x3::xs-> true
  | _ -> false  
    
let len_lt_3 (xs:'a list) : bool =
  match (len_ge_3 xs) with 
    true-> false 
  | _ -> true
    
let len_comp_3 (xs:'a list) : int =
  match (len_eq_3 xs) with 
    true->0
  | _ -> if(len_ge_3 xs) then 1
      else -1
    

  

