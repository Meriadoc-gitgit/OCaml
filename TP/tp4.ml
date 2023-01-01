(* Definition de type abr *)
type 'a btree = 
| Empty 
| Node of ('a * 'a btree * 'a btree)

let bt = Node(5, Node(2, Node(1, Empty, Empty), Node(4, Node(3, Empty, Empty), Empty)), Node(6, Empty, Node(7, Empty, Node(8, Empty, Empty))))

(* Exo 1 - Test d'ABR *)
let rec lt_btree (bt : 'a btree) (x : 'a) : bool = 
  match bt with
  | Empty -> true
  | Node (a,g,d) -> if a > x then false else a <= x && lt_btree g x && lt_btree d x;;

let rec ge_btree(bt : 'a btree) (x : 'a) : bool = 
  match bt with 
  | Empty -> true
  | Node(a,g,d) -> if a <= x then false else a > x && ge_btree g x && ge_btree d x;;

let rec is_abr (bt : 'a btree) : bool = 
  match bt with 
  | Empty -> true
  | Node (a,g,d) -> if ge_btree d a = true && lt_btree g a = true && is_abr g && is_abr d then true else false;;



(* Exo 2 - Recherche dans un ABR *)
let rec mem (bt : 'a btree) (x : 'a) : bool = 
  match bt with 
  | Empty -> false 
  | Node (a,g,d) -> a = x || mem g x || mem d x;;



(* Exo 3 - Construction d'ABR *)
let rec insert (bt : 'a btree) (x : 'a) : 'a btree = 
  match bt with 
  | Empty -> Node (x, Empty, Empty)
  | Node (a, Empty, Empty) -> if a <= x then Node (a, Empty, Node(x,Empty,Empty)) else Node (a, Node (x,Empty,Empty), Empty)
  | Node (a,g,d) -> 
    if a > x then Node (a, insert g x, d) 
    else Node(a, g, insert d x);;

let rec abr_of_list (l : 'a list) : 'a btree = 
  match List.rev l with 
  | [] -> Empty
  | e :: r -> insert (abr_of_list (List.rev r)) e;;


(* Exo 4 - Tri par ABR *)
let rec list_of_abr (bt : 'a btree) : 'a list = 
  match bt with
  | Empty -> []
  | Node (a,g,d) -> list_of_abr g @ a :: list_of_abr d;;

let abr_sort (l : 'a list) : 'a list = 
  list_of_abr (abr_of_list l);;