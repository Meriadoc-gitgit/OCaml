(* 1. Somme des premiers entiers naturels impairs *)
let rec sum_impairs (n : int) : int = 
  if n = 0 then 0
  else sum_impairs (n-1) + 2*n-1;;


(* 2. Iteration d'une fonction *)
let rec iter_f (n : int) (f : 'a -> 'a) (x : 'a) : 'a = 
  if n=0 then x 
  else f (iter_f (n-1) f x);;


(* 3. Concatenation de listes *)
let rev_append (l1 : 'a list) (l2 : 'a list) : 'a list = 
  let rec rev (l : 'a list) : 'a list = 
    match l with
    | [] -> [] 
    | e :: r -> (rev r) @ (e :: [])
  in (rev l1) @ l2;;

let rec rev (l : 'a list) : 'a list = 
  match l with 
  | [] -> []
  | e :: r -> rev r @ (e :: []);;

let append (l1 : 'a list) (l2 : 'a list) : 'a list = l1 @ l2;;


(* Interateurs sur les listes *)
let rec map (f : 'a -> 'b) (l : 'a list) : 'b list = 
  match l with 
  | [] -> []
  | e :: r -> f e :: map f r ;;

let rec filter (f : 'a -> bool) (l : 'a list) : 'a list = 
  match l with 
  | [] -> []
  | e :: r -> if f e then e :: filter f r else filter f r;;

let rec partition (f : 'a -> bool) (l : 'a list) : 'a list * 'a list = 
  let res (f : 'a -> bool) (t : 'a list * 'a list) (v : 'a) = 
    if f v then (v :: fst t, snd t) else (fst t, v :: snd t)
  in 
  match l with 
  | [] -> ([],[])
  | e :: r -> res f (partition f r) e;;

let rec fold_right (f : 'a -> 'b -> 'b) (l : 'a list) (b : 'b) : 'b = 
  match rev l with 
  | [] -> b
  | e :: r -> (f e b) + fold_right f (rev r) b;;