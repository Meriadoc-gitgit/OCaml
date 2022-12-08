(* Fonctions - Listes *)

(* Exo 1 - PGCD *)
let rec pgcd (a : int) (b : int) : int = 
  if (a < b) then raise (Invalid_argument "a doit etre superieur a b")
  else if (a mod b = 0) then b
  else (pgcd b (a mod b));;



(* Exo 2 - Quelques fonctions sur les listes *)
(* Fonction construit la liste contenant n occurences de e *)
let rec repeat (n : int) (e : 'a) : 'a list = 
  if n = 0 then []
  else e :: repeat (n - 1) e;;

(* Fonction renvoie la liste de valeur absolue *)
let rec abs_list (l : int list) : int list = 
  match l with
  | [] -> []
  | e :: r -> (abs e) :: (abs_list r);;

(* Fonction determine le plus grand entier d'une liste non vide *)
let rec max_list (l : int list) : int = 
  match l with 
  | [] -> raise (Invalid_argument "empty list")
  | x :: r -> 
    if r = [] then x 
    else (max x (max_list r));;

(* Enlever la derniere element d'une liste *)
let rec rm_last (l : 'a list) : 'a list = 
  match l with
  | [] -> raise (Invalid_argument "empty list")
  | [x;y] -> [x]
  | x :: r -> x :: (rm_last r);;

(* Contruit la liste des entiers consecutifs de l'intervalle [a,b] *)
let rec range_inter (a : int) (b : int) : int list = 
  if a > b then []
  else a :: range_inter (a + 1) b;;

(* Contruit la liste de listes obtenue en ajoutant en tete de chaque liste de l l'element e *)
let rec map_cons (e : 'a) (l : 'a list list) : 'a list list = 
  match l with 
  | [] -> []
  | y :: r -> (e :: y) :: (map_cons e r);;




(* Exo 3 - Prefixes d'une liste *)
(* Construit une liste obtenur en supprimant les n premiers elements de l *)
let rec rm_pref (n : int) (l : 'a list) : 'a list = 
  if n = 0 then l
  else match l with
  | [] -> []
  | x :: r -> rm_pref (n-1) r;;

(* Calcule le nombre d'elements identiques au debut de la liste l *)
let rec lg_prefix (l : 'a list) : int = 
  match l with
  | [] -> 0
  | [x] -> 1
  | x :: y :: r -> if x != y then 1 else 1 + lg_prefix(y :: r);;


(* Construit la liste de toutes les listes correspondant a un prefixe de l *)
let rec prefixes (l : 'a list) : (('a list) list) =
  match l with 
  | [] -> [[]]
  | h :: t -> ([] :: map_cons h (prefixes t));;
