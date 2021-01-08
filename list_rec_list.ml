(* GRADE:  100% *)
let rec intercale1 (z:'a) (xs:'a list) : 'a list = 
  match xs with 
    []->[]
  |x::[]->x::[] 
  |x1::x->x1::z::(intercale1 z x) 
    
    
let rec intercale2 (z:'a) (xs:'a list) : 'a list =
  match xs with
    []->z::[]
  |x1::x->z::x1::(intercale2 z x)
    
let rec begaie (xs:'a list) : ('a list) =
  match xs with
    []->[]
  |x1::x->x1::x1::begaie x

let rec oublie1 (xs:'a list) : ('a list) =
  match xs with
    []->[]
  |x1::[]->x1::[]
  |x1::x2::x->x1::oublie1 x

let rec oublie2 (xs:'a list) : ('a list) =
  match xs with
    []->[]
  |x1::[]->[]
  |x1::x2::x->x2::oublie2 x



  

