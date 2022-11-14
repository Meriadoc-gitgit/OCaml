(* Definition et Application de fonction *)

(* Exo 1 *)
let double (x : 'a) : 'a = 2 * x;;

let somme_2double (x : 'a) : 'a = 3 * (double x);;

let make_even (x : int) : int =
  if (x mod 2 = 0) then x 
  else (double x);;


(* Exo 3 *)
(* Somme des n premiers entiers naturels impairs *)
let rec sum_impairs (n : int) : int =
  if (n = 0) then 0
  else (sum_impairs (n - 1)) + 2*n - 1;;

(* Somme de tous les entiers naturels impairs strictement inferieurs a n *)
let rec sum_impairs_inf ( n : int ) : int = 
  if ( n mod 2 = 0 ) then sum_impairs ( n / 2 ) 
  else sum_impairs ((n - 1) / 2) ;;


(* Exo 4 - Suite recurrente *)
(* Suite (Un) =
      U0 = 2
      U_n+1 = 3Un + 4 *)
(* Fonction calcule le n-ieme terme Un de la suite *)
let rec u_n (n : int) : int = 
  if n = 0 then 2
  else 3*(u_n (n - 1)) + 4;;

(* Fonction calcule la somme des n premiers termes de la suite *)
let rec sum_un (n : int) : int = 
  if n = 0 then 0 
  else (u_n (n - 1)) + (sum_un (n - 1));;


(* Exo 5 - Representation binaire d'un entier naturel *)
(* Fonction calcule le nombre de l contenus dans la representation binaire d'un entier n *)
let rec nb_un (n : int) : int =
  if n = 1 then 1
  else if (n mod 2 = 0) then 
    if ((n - n/2) mod 2 = 0) then nb_un (n/2) 
    else nb_un (n/2) + 1
  else nb_un (n - 1);;

(* Fonction calcule le nombre de bits minimum contenus dans la representation binaire d'un entier n *)
let rec nb_bits (n : int) : int =
  if n = 1 then 1
  else if (n mod 2 = 0) then 1 + nb_bits (n/2)
  else 1 + nb_bits ((n - 1)/2);;

(* Fonction calcule le plus grand entier naturel que l'on peut representer avec n>0 bits *)
let rec nb_max (n : int) : int = 
  if n = 1 then 1
  else 1 + 2*(nb_max (n - 1));;