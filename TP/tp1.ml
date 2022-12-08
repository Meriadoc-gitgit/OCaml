(* TP1 *)

(* Exo 1 - Representation decimale d'un entier naturel *)
(* Somme des chiffres de la representation decimal d'un entier n *)
let rec sum_chiffres (n : int) : int = 
  if (n < 10) then n
  else n mod 10 + sum_chiffres (n/10)

(* Nombre de chiffres significants contenus dans la representation decimale d'un entier n *)
let rec nb_chiffres (n : int) : int = 
  if (n < 10) then 1
  else 1 + nb_chiffres (n/10)



(* Exo 2 - Nombres premiers *)
(* Calcule le plus petit diviseur de n compris entre i (inclus) et n (exclus) *)
let rec less_divider (i : int) ( n : int) : int = 
  if i >= n then 0
  else if n mod i = 0 then i
  else less_divider (i + 1) n

(* Determine si un entier naturel n est un nombre premier *)
let prime (n : int) : bool = 
  if n = 0 || n = 1 then false 
  else if less_divider 2 n = 0 then true
  else false 

(* le plus petit nombre premier superieur ou egal a l'entier naturel n *)
let rec next_prime (n : int) : int = 
  if (prime (n+1)) then n + 1 (* Next prime, kphai prime =))) *)
  else next_prime (n + 1)

(* n-ieme nombre premier *)
let rec nth_prime (n : int) : int = 
  if n = 0 then 2
  else next_prime (nth_prime (n-1))