(* Type des etiquettes *)
type 'a label_exparith = 
| L_var of 'a 
| L_Cste of int 
| L_Plus 
| L_Mult 
| L_Div 
| L_opp ;;

(* Type des arbres binaires etiquetes par des valeurs de type 'a label_exparith *)
type 'a btree = 
| Empty 
| Node of 'a * ('a btree) * ('a btree);;

type 'a exparith_tree = ('a label_exparith) btree;;

(* exemple *)
let e = 
  Node(L_Mult,
    Node(L_Plus,
      Node(L_Cste 2, Empty, Empty),
      Node(L_opp,
        Node(L_var "x", Empty, Empty),
        Empty)),
    Node(L_var "y", Empty, Empty));;




(* Verification qu'un arbre binaire represente une expression arithmetique *)
let rec wellformed (e : 'a exparith_tree) : bool = 
  match e with 
  | Empty -> false 
  | Node(e,g,d) -> 
    match e with 
    | L_var(x) -> g = Empty && d = Empty
    | L_Cste(k) -> g = Empty && d = Empty
    | L_opp -> wellformed g && d = Empty
    | _ -> wellformed g && wellformed d;;



(* Expression arithmetique != Etiquetes *)
(* Opp, Plus, Mult, Div sont des constructeurs d'expressions arithmetiques a partir d'autres expressions arithmetiques *)
(* This one simplifie the previous one *)
type 'a exparith = 
| Var of 'a 
| Cste of int
| Opp of 'a exparith 
| Plus of 'a exparith * 'a exparith 
| Mult of 'a exparith * 'a exparith
| Div of 'a exparith * 'a exparith;;

(* Exemple *)
(* Vi may operation co nhieu hon 1 bien nen can dau ngoac, chu con lai 1 bien thi khong can *)
let ex = 
  Mult (Plus(Cste 3, Opp(Var "x")), Var "y");;

(* Nombre d'occirences de symboles de variable dans une epression arithmetique *)
let rec nb_var (e : 'a exparith) : int = 
  match e with
  | Var x -> 1
  | Cste n -> 0
  | Opp e0 -> nb_var e0 
  | Plus (e1,e2) -> nb_var e1 + nb_var e2
  | Mult (e1,e2) -> nb_var e1 + nb_var e2 
  | Div (e1,e2) -> nb_var e1 + nb_var e2;;

(* Evaluation d'une expression arithmetique *)
let rec eval_e (evn : ('a * int) list) (e : 'a exparith) : int = 
  match e with
  | Var x -> List.assoc x evn
  (* Phai viet the nay va de try...with vao 1 func rieng vi neu khong se bi non-exhaustive!! *)
  | Cste n -> n
  | Opp e0 -> -(eval_e evn e0)
  | Plus (e1,e2) -> (eval_e evn e1) + (eval_e evn e2)
  | Mult (e1,e2) -> (eval_e evn e1) * (eval_e evn e2) 
  | Div (e1,e2) -> (eval_e evn e1) / (eval_e evn e2);;

(* define example for list of product type *)
let envxy = [("x",2);("y",5)];;

(* Autre version *)
let eval_var (env : (string*int) list) (x : string) : int = 
  try List.assoc x env with
  | Not_found -> raise (Invalid_argument (x ^ " indefini"));;

let rec eval_e_wtry (evn : ('a * int) list) (e : 'a exparith) : int = 
    match e with
    | Var x -> eval_var evn x (* Idk but it didn't fucking work =)) *)
    | Cste n -> n
    | Opp e0 -> -(eval_e evn e0)
    | Plus (e1,e2) -> (eval_e evn e1) + (eval_e evn e2)
    | Mult (e1,e2) -> (eval_e evn e1) * (eval_e evn e2) 
    | Div (e1,e2) -> (eval_e evn e1) / (eval_e evn e2);;






(* Arbres generaux *)
(* Arbre general = arbre non vide dans lequel chaque noeud a un nombre quelconque de sous-arbres
   A chaque noeud est associe :
   - une etiquette
   la liste de ses sous-arbres *)

(* Definition OCaml du type des arbres generaux *)
(* No Empty var*)
type 'a gtree = GNode of 'a * (('a gtree) list);;

(* exemple *)
let gt = 
  GNode(5, [GNode(3, [GNode (9,[]); GNode(1,[])]);
            GNode(2,[]);
            GNode(4, [GNode(7,[]);GNode(6,[]);
                      GNode(8,[]);GNode(0,[])])]);;