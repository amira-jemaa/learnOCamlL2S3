(* GRADE:  75% *)
(* Q.2 *)

let xor (a : bit) (b : bit) : bit =
  match a,b with
    false,true->true
  |true,false->true
  |_,_->false
(* Q.3 *)

let half_adder (a : bit) (b : bit) : (bit * bit) =
  match a,b with
    false,true->(true,false)
  |true,false->(true,false)
  |true,true->(false,true)
  |false,false->(false,false)
(* Q.4 *)

let adder (a:bit) (b:bit) (c:bit): duet=
  let (s1,r1)= (half_adder a b) in
		let (s2,r2)= (half_adder c s1) in
		(s2, ((r1 || r2) && (not(r1&&r2))) ) 
