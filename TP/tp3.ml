(* Exo1 *)
(* Determiner si e appartient a l'ensemble represente par l *)
let rec is_in (e : 'a) (l : 'a list) : bool =
  match l with 
  | [] -> false
  | x :: r -> if x = e then true else (is_in e r);;

(* ajouter elem si elem pas dans l *)
let add_elem (e : 'a) (l : 'a list) : 'a list = 
  if is_in e l then l 
  else e :: l;;


(* Exo2 - Sous-ens et egalite d'ensemble *)
let rec is_subset_rec (l1 : 'a list) (l2 : 'a list) : bool = 
  match l1 with
  | [] -> true
  | x :: r -> (is_in x l2) && (is_subset_rec r l2);;

let is_subset (l1 : 'a list) (l2 : 'a list) : bool = 
  List.for_all (fun x -> is_in x l2) l1;;

(* Determiner si les 2 ens sont egaux *)
let eq_set (l1 : 'a list) (l2 : 'a list) : bool = 
  is_subset l1 l2 && List.length l1 = List.length l2 ;;



(* Exo 3 - Intersection *)
let rec intersection_rec (l1 : 'a list) (l2 : 'a list) : 'a list = 
  if l2 = [] then []
  else match l1 with
  | [] -> []
  | x :: r -> if is_in x l2 then x :: intersection_rec r l2 else intersection_rec r l2;;

let intersection (l1 : 'a list) (l2 : 'a list) : 'a list = 
  List.filter (fun x -> is_in x l2) l1;;



(* Exo 4 - Union *)
let rec union_rec (l1 : 'a list) (l2 : 'a list) : 'a list = 
  match l1 with 
  | [] -> l2 
  | x :: r -> union_rec r (add_elem x l2);;

let union_left (l1 : 'a list) (l2 : 'a list) : 'a list = 
  List.fold_left (fun (a : 'a list) (b : 'a) -> add_elem b a) l1 l2;;

let union_right (l1 : 'a list) (l2 : 'a list) : 'a list = 
  List.fold_right (fun (a : 'a) (b : 'a list) -> add_elem a b) l1 l2;;



(* Exo5 - Produit cartesien *)
(* Construit la liste de couple *)
let make_pairs (x : 'a) (l : 'b list) : ('a * 'b) list = 
  List.map (fun (b : 'b) : ('a * 'b) -> (x, b)) l;;

let rec product_rec (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list = 
  if l2 = [] then []
  else match l1 with 
  | [] -> []
  | x :: r -> (make_pairs x l2) @ product_rec r l2;; 

let product (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list = 
  List.flatten (List.map (fun (a : 'a) : ('a * 'b) list -> make_pairs a l2) l1);;


