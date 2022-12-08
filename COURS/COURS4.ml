(* Type somme sur List.assoc *)
let assoc_opt (k : 'a) (l : ('a * 'b) list) : 'b option =
  try Some (List.assoc k l) with
  | Not_found -> None

(* Retourner une valeur par defaut dflt si aucune valeur associee *)
let assoc_dflt (k : 'a) (dflt : 'b) (l : ('a * 'b) list) : 'b = 
  match assoc_opt k l with
  | None -> dflt
  | Some x -> x




(* ========================================
   ========================================
   ======================================== *)
type 'a btree = Empty | Node of 'a * ('a btree) * ('a btree)

(* Etiquette de la racine d'un arbre binaire non vide *)
let label_root (t : 'a btree) : 'a = 
  match t with
  | Empty -> raise (Invalid_argument "label_root")
  | Node (e,_,_) -> e 

(* Sous-arbres gauche *)
let sag (t : 'a btree) : 'a btree = 
  match t with
  | Empty -> raise (Invalid_argument "sag")
  | Node (_,g,_) -> g

(* Sous-arbres droite *)
let sad (t : 'a btree) : 'a btree = 
  match t with
  | Empty -> raise (Invalid_argument "sad")
  | Node (_,_,d) -> d

(* Hauteur *)
let rec height (t : 'a btree) : int = 
  match t with
  | Empty -> 0
  | Node (_, g, d) -> 1 + max (height g) (height d)

(* Nb de noeuds - Taille d'un arbre binaire *)
let rec size (t : 'a btree) : int = 
  match t with 
  | Empty -> 0
  | Node (_, g, d) -> 1 + (size g) + (size d)

(* Recherche etiquette *)
let rec mem (x : 'a) ( t : 'a btree) : bool = 
  match t with
  | Empty -> false
  | Node (e, g, d) -> x = e || mem x g || mem x d

(* Parcours en profondeur d'un arbre binaire *)
(* pre(Empty) = eps 
   pre(Node (e,g,d)) = e . pre(g) . pre(d) *)
let rec pre (t : 'a btree) : 'a list = 
  match t with
  | Empty -> []
  | Node (e, g, d) -> e :: ((pre g) @ (pre d));;

(* Parcours en largeur d'un arbre binaire *)
(* Truc complexe :( *)
let breadth (t : 'a btree) : 'a list = 
  let rec aux acc lt = 
    match lt with 
    | [] -> List.rev acc 
    | Empty :: tlt -> aux acc tlt 
    | Node (e, g, d) :: tlt -> aux (e :: acc) (tlt @ [g;d])
  in 
  aux [] [t];;