(* GRADE:  100% *)
(* Q1 *)
let nb_ingredients (rdic:dico) (r:string) : int = 
  let recette = List.assoc r rdic in 
  List.length recette 

(* Q2 *)
let rec recette_avec (rdic:dico) (i:string) : string list = 
  match rdic with
    [] -> [] 
  | x :: xs -> match x with 
      (rct,ingrd) -> if List.mem i ingrd then rct::recette_avec xs i 
        else recette_avec xs i 


(* Q3 *)
let rec recette_sans (rdic:dico) (i:string) : string list =
  match rdic with 
    [] -> [] 
  | x :: xs -> match x with 
      (rct,ingrd) -> if not(List.mem i ingrd) then rct::recette_sans xs i 
        else recette_sans xs i 

(* Q4 *)
let rec union (x: 'a list) (y : 'a list) : 'a list = 
  match x with
    [] -> y
  | x :: xs -> if not(List.mem x y) then union xs (x :: y) 
      else union xs y 

(* Q5 *)
let rec tous_ingredients (rdic:dico) : string list = 
  match rdic with 
    []->[] 
  | x :: xs -> let (rct,ingrd)=x in union (tous_ingredients xs) ingrd 

(* Q6 *)
let dico_ingredients (rdic : dico) : dico = 
  let ing = (tous_ingredients rdic) in
  let rec loop lin r = match lin with 
      [] -> r 
    | x :: xs -> (x,recette_avec rdic x) :: loop xs r in loop ing [] 


(* Q7 *)
let ingredient_principal (idic : dico) : string = 
  if idic=[] then raise (Invalid_argument "idic")
  else let rec loop l res t = 
         match l with
           [] -> res 
         | x :: xs -> let (name,recette)=x in let len = (List.length recette) in 
             if len >= t then loop xs name len 
             else loop xs res t 
    in loop idic "" 0 

