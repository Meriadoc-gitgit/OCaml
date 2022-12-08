(* Fonction depuis TD2 *)
(* Contruit la liste de listes obtenue en ajoutant en tete de chaque liste de l l'element e *)
let rec map_cons (e : 'a) (l : 'a list list) : 'a list list = 
  match l with 
  | [] -> []
  | y :: r -> (e :: y) :: (map_cons e r);;

(* Construit la liste de toutes les listes correspondant a un prefixe de l *)
let rec prefixes (l : 'a list) : (('a list) list) =
  match l with 
  | [] -> [[]]
  | h :: t -> ([] :: map_cons h (prefixes t));;


(* =============================================
   =============================================
   ============================================= *)



(* Frequences d'apparition des elements d'une liste *)
let rec add_freq (c : 'a) (l : ('a * int) list) =
  match l with
  | [] -> [(c, 1)]
  | h :: t -> 
    if c = fst h then (c, (snd h) + 1) :: t 
    else h :: (add_freq c t);;

let rec freq (l : 'a list) : ('a * int) list =
  match l with 
  | [] -> []
  | h :: t -> (add_freq h (freq t));;

let exists_freq_above (max : int) (l : ('a * int) list) : bool = 
  List.exists (fun (x : 'a * int) -> snd x > max) l;;



(* Quelques fonctions sur les listes *)
let sum_left (l : int list) : int = 
  List.fold_left (+) 0 l;;

let sum_right (l : int list) : int = 
  List.fold_right (+) l 0;;



(* Listes bien balisees *)
let add_fst (c : (int * int)) : (int * int) = 
  let (x, y) = c in (x + 1, y);;

let add_snd (c : (int * int)) : (int * int) = 
  let (x, y) = c in (x, y + 1);;


let rec nb_of (l : char list) : (int * int) = 
  match l with 
  | [] -> (0,0)
  | h :: t -> 
    if h = '<' then add_fst (nb_of t)
    else add_snd (nb_of t);; 

let o_sup_f (l : char list) : bool = 
  let (x, y) = nb_of l in x >= y;;

let all_o_sup_f (l : (char list) list) : bool =
  List.for_all o_sup_f l;;

let dyck (l : char list) : bool = 
  all_o_sup_f (prefixes l) && (let (o,f) = nb_of l in o = f);;