(* Exo1 - Types sommes et exceptions *)

type value = B of bool | I of int;;

exception TYPE_ERROR of int;;

let not1 (v : value) : value = 
  match v with
  | B v -> B (not v) 
  | I v -> raise (TYPE_ERROR v);;

let not2 (v : value) : value = 
  match v with 
  | B b -> not1 (B b)
  | I i -> if i = 0 then B true else B false;;

exception DIV_BY_0 of int;;

let div1 (v1 : value) (v2 : value) : value = 
  match v1 with 
  | B b1 -> raise (Invalid_argument "div")
  | I i1 -> 
    match v2 with 
    | I i2 -> if i2 = 0 then raise (DIV_BY_0 i1) else I (i1 / i2)
    | B b2 -> raise (Invalid_argument "div");;

let div2 (v1 : value) (v2 : value) : value option = 
  match v1 with 
  | B b1 -> raise (Invalid_argument "div")
  | I i1 -> 
    match v2 with 
    | I i2 -> if i2 = 0 then None else Some(I (i1 / i2))
    | B b2 -> raise (Invalid_argument "div");;


(* Exo2 - Branches d'un arbre binaire *)

type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;

(* Exemple *)
let t_ex = Node(5, Node(3, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(2, Node(3, Empty, Node(7, Empty, Empty)), Empty));;

let rec max_length_branch_rec (t : 'a btree) : 'a list = 
  match t with 
  | Empty -> []
  | Node (a, g, d) -> 
    let bg = max_length_branch_rec g in 
    let bd = max_length_branch_rec d in 
    if (List.length bg >= List.length bd) then a :: bg 
    else a :: bd ;;


(* Non rec version *)
let max_length_branch (t : 'a btree) : 'a list = 
  let rec aux (t : 'a btree) : 'a list * int = 
    match t with 
    | Empty -> ([], 0)
    | Node (a, g, d) -> 
      let bg, lg = aux g in 
      let bd, ld = aux d in 
      if lg >= ld then (a :: bg, lg + 1)
      else (a :: bd, ld + 1)
    in fst (aux t);;



(* (Something I haven't really understand yet) *)
let rec max_flow_branch (t : 'a btree) : 'a list = 
  match t with 
  | Empty -> []
  | Node(x,Empty,Empty) -> [x] (* Optionnel *)
  | Node(x,g,Empty) -> x :: max_flow_branch g
  | Node(x,Empty,d) -> x :: max_flow_branch d
  | Node(x,g,d) -> 
    let Node(y,_,_) = g in 
    let Node(z,_,_) = d in
    if y >= z then x :: max_flow_branch g else x :: max_flow_branch d;; (* It's sucks but it works =))) *)




(* Exo3 - Profondeur d'une etiquette *)
let rec at_prof (n : int) (x : 'a) (t : 'a btree) : bool = 
  match t with 
  | Empty -> false
  | Node(a, g, d) -> 
    if n = 0 then x = a
    else at_prof (n-1) x g || at_prof (n-1) x d;;

let rec at_prof_list (n : int) (t : 'a btree) : 'a list = 
  match t with 
  | Empty -> []
  | Node(a,g,d) -> if n = 0 then [a] else at_prof_list (n-1) g @ at_prof_list (n-1) d;;


(* Another one I haven't understand yet *)
let rec etiq_prof_list (x : 'a) (t : 'a btree) : int list = 
  match t with 
  | Empty -> []
  | Node(a,g,d) -> 
    let tmp = etiq_prof_list x g @ etiq_prof_list x d in 
    let s = List.map (fun z -> z + 1) tmp in
    if x = a then 0::s else s;;

let rec prof_max (t : 'a btree) : int = 
  match t with 
  | Empty -> -1
  | Node(a,g,d) -> 1 + max(prof_max g) (prof_max d);;

let max_prof_etiq (x : 'a) (t : 'a btree) : int = 
  let liste = etiq_prof_list x t in 
  List.fold_left max (-1) liste;;