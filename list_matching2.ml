(* GRADE:  100% *)
let snd (xs:'a list) : 'a =
  match xs with 
    x1::x2::xs -> x2
  |_->raise Not_found
    
let swap_hd_snd (xs:'a list) : 'a list =
  match xs with 
    x1::x2::xs->x2::x1::xs
  |_->xs
    
let hd_0 (xs:int list) : bool = 
  match xs with 
    x1::xs->if(x1=0) then true
      else false
  |_ -> false
    
let eq_hd (x0:'a) (xs:'a list) : bool =
  match xs with 
    x1::xs->if(x1=x0) then true
      else false
  |_->false
    
    

  

