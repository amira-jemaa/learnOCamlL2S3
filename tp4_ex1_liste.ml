(* GRADE:  100% *)
(* Q1 *)
let rec drop (n: int) (xs : 'a list) : 'a list = 
  if n<=0 then xs else 
    match xs with 
      [] -> [] 
    | x :: xs -> if n = 0 then xs else 
          drop(n-1)(xs) 

(* Q2 *)
let rec take (n : int) (xs : 'a list) : 'a list = 
  if n >= List.length xs
  then xs else 
  if n <= 0 then []
  else match xs with
      [] -> []
    | x :: xs -> if n==0 then xs 
        else x :: take(n-1) xs 

(* Q3 *)
let rec sub xs start len = 
  if start == 0 then take len xs 
  else if start < 0 then match xs with 
      [] -> xs 
    | _ -> take ( abs (start) ) xs 
  else match xs with
      [] -> xs 
    | x :: xs -> if start != 0 then sub xs (start-1) len
        else
          take len xs 
