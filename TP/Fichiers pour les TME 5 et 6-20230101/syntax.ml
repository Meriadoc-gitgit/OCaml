(** expressions *)
type 'a exparith =  
  | Var of 'a 
  | Cste of int
  | Opp  of 'a exparith
  | Plus of 'a exparith * 'a exparith
  | Mult of 'a exparith * 'a exparith
  | Div  of 'a exparith * 'a exparith

(*---------------------------------------------------------------*)
(** comparison operators *)
type comp_op2 = 
  | Eq  (* = *) | Neq (* !$\neq$! *)
  | Leq (* !$\leq$! *) | Lt  (* < *) 
  | Geq (* !$\geq$! *) | Gt  (* > *)

(*---------------------------------------------------------------*)
(** boolean expressions *)
type 'a expbool = 
  | CompArith of comp_op2 * 'a exparith * 'a exparith
  | Not of 'a expbool
  | And of 'a expbool  * 'a expbool
  | Or  of 'a expbool  * 'a expbool

(*---------------------------------------------------------------*)
(** programs *)
type 'a program =
  | Assign of 'a * 'a exparith
  (* `Assign (x,e)` est `x <- e`  *)

  | If of 'a expbool * 'a program * 'a program
  (* `If (b,p1,p2)` est `if b then { p1 } else { p2 }`  *)

  | Seq of 'a program list
  (* `Seq [p1, …, pn]` est `p1; …; pn`  *)

  | While of 'a expbool * 'a program
  (* `While (b,p)` est `while (b) { p }`  *)

  | For of 'a * int * int * 'a program
  (* `For (x,i,j,p)` est `for (x = i to j) { p }` 
     Avec la convention que si `j <= i`, alors la boucle ne fait rien. *)
