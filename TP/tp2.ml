(* Importation de code du TD *)
(* Construit une liste obtenue en supprimant les n premiers elements de l *)
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

(* Contruit la liste des entiers consecutifs de l'intervalle [a,b] *)
let rec range_inter (a : int) (b : int) : int list = 
  if a > b then []
  else a :: range_inter (a + 1) b;;

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


  


(* =============================================== *)

(* Exo 1 - Suite de listes *)
(* Construction de L_k a partir de L_k-1 *)
let rec next_list (l : int list) : int list = 
  match l with
  | [] -> []
  | e :: r -> (lg_prefix (e :: r)) :: e :: next_list (rm_pref (lg_prefix (e :: r)) (e :: r));;

(* Construit la liste L_k a partir de k *)
let rec make_liste_k (k : int) : int list = 
  if k = 1 then [1]
  else if k = 2 then next_list (make_liste_k 1)
  else next_list (make_liste_k (k-1));;



(* Exo 2 - Crible d'Eratosthene *)
(* Construit la liste des entiers consecutifs de 2 a n *)
let rec genere_list (n : int) : int list = 
  if n < 2 then []
  else range_inter 2 n;;

(* Contruit la liste des elements de l privee des multiples de n *)
let rec elimine (l : int list) (n : int) : int list =
  match l with
  | [] -> []
  | [e] -> if e mod n = 0 then [] else [e]
  | e :: y :: r -> if e mod n = 0 then elimine (y :: r) n else e :: elimine (y :: r) n;; 

(* Construit la liste des elements de l en ne conservant que les entiers ayant la propriete de n'etre multiple d'aucun des entiers qui les precedent dans l *)
let rec ecreme (l : int list) : int list = 
  match l with 
  | [] -> []
  | e :: r -> e :: ecreme (elimine r e);;

(* Construit la liste des nombres premiers inf ou egaux a n *)
let crible (n : int) : int list = 
  let l = genere_list n 
in ecreme l;;



(* Exo 3 - Equations diophatiennes a une inconnue *)

(* 1. Representation d'un polynome *)

(* Calcule le degre d'un polynome *)
let degre (p : int list) : int = 
  List.length p - 1;;

(* Calcule ??? *)
let rec value_of (x : int) (p : int list) : int = 
  match List.rev p with 
  | [] -> 0
  | e :: r -> 
    let rec loop (x : int) (n : int) : int = 
      if n = 0 then 1
      else
        let i = n in
        if i > 0 then x * loop x (i - 1)
        else 1 in 
    e * (loop x (List.length r)) + value_of x (List.rev r);; 

(* Determine si x est racine du polynome representee par p *)
let is_root (x : int) (p : int list) : bool = 
  if value_of x p = 0 then true
  else false;; 



(* Racine entieres d'un polynome *)
let upper_bound (p : int list) : int = 
  degre p * max_list (abs_list p);;

(* Construit la liste des elements de la liste l qui sont racines entieres du polynome represente par p *)
let rec filter_roots (l : int list) (p : int list) : int list = 
  match l with
  | [] -> []
  | e :: r -> if is_root e p then e :: (filter_roots r p) else (filter_roots r p);;

(* Construit la liste des racines entiers d'un polnome *)
let roots_list (p : int list) : int list = 
  let q = range_inter 2 (upper_bound p) in 
  filter_roots q p;;
