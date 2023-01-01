open Syntax
open Input
open Printer

let rec eval_e (env:('a* int) list) (e:'a exparith) : int =
  match e with
  | Var(x) -> List.assoc x env
  | Cste(n) -> n
  | Opp e -> - (eval_e env e) 
  | Plus(e1,e2) -> (eval_e env e1) + (eval_e env e2)
  | Mult(e1,e2) -> (eval_e env e1) * (eval_e env e2)
  | Div (e1,e2) -> (eval_e env e1) / (eval_e env e2)

let env = [("x", 2); ("y", 5); ("z", -1)]

let () = assert (eval_e env (parse_expr "x + y") = 7 )
let () = assert (eval_e env (parse_expr "x * y") = 10)
let () = assert (eval_e env (parse_expr "x * 3") = 6 )

(*==================================================================*)
(*==================================================================*)
(** Exo 1: Expressions booléennes *)

(*------------------------------------------------------------------*)
let eval_comp_op2 (op : comp_op2) (x : int) (y : int) : bool =
  assert false (* TODO *)

(*
let () = assert (eval_comp_op2 Leq 1 5        )
let () = assert (eval_comp_op2 Leq 5 1 = false)
let () = assert (eval_comp_op2 Geq 1 5 = false)
let () = assert (eval_comp_op2 Geq 5 1        )
*)

(*------------------------------------------------------------------*)
let eval_b (env : ('a * int) list) (b : 'a expbool) : bool =
  assert false (* TODO *)

(*
let () = assert (eval_b env (parse_expr_b "x <= 2")         )
let () = assert (eval_b env (parse_expr_b "x <= 1") = false )
let () = assert (eval_b env (parse_expr_b "(y - z) = 3 * x"))
*)

(*==================================================================*)
(*==================================================================*)
(** Exo 2: Programmes *)

(*---------------------------------------------------------------*)
let skip = (* : 'a program *) 
  fun () -> assert false (* TODO *)


(*---------------------------------------------------------------*)
let rec change_assoc (k : 'a) (v : 'b) (l : ('a * 'b) list) : ('a * 'b) list =
  assert false (* TODO *)

(*
let () = assert (change_assoc "x" 5 [] = [("x",5)])
let () = assert (change_assoc "x" 5 [("x",3)] = [("x",5)])
let () = assert (change_assoc "x" 5 [("y",42)] = [("y",42); ("x",5)])
let () = assert (change_assoc "x" 5 [("y",42); ("x", 1); ("z", 3)]
                 =                  [("y",42); ("x", 5); ("z", 3)])
*)

(*---------------------------------------------------------------*)
let rec eval_p (env : ('a * int) list) (p : 'a program) : ('a * int) list =
  assert false (* TODO *)


(*------------------------------------------------------------------*)
let check_ret (e : (string * int) list) (expected : int) : bool =
  assert false (* TODO *)


(*
let () = assert (check_ret [("x",10); ("ret", 8)] 8)
let () = assert (check_ret [("x",10); ("ret", 8)] 0 = false)
*)

(*------------------------------------------------------------------*)
let test_eval (expected : int) (p : string program) : bool =
  assert false (* TODO *)


let test_eval_str (expected : int) (s : string) : bool =
  test_eval expected (parse_prog s)

(*
let () =
  assert (test_eval_str 2 "x <- 1; y <- 2; ret <- y")

let () =
  assert (test_eval_str 3 "x <- 1; y <- 2; ret <- x + y")

let () =
  assert (test_eval_str 2 
            "x <- 1; if x = 1 then ret <- 2 else ret <- 3")

let () =
  assert (test_eval_str 3 
            "x <- 2; if x = 1 then ret <- 2 else ret <- 3")

let () = 
  assert (test_eval_str 55 
            "x <- 10;
             r <- 0;
             while x > 0 {
               r <- x + r;
               x <- x-1
             };
             ret <- r")

let () = 
  assert (test_eval_str 120 
            "fact <- 0;
             for (x = 0 to 6) {
               if x = 0 then
                 fact <- 1 
               else
                 fact <- x * fact
             };
             ret <- fact")

let () = 
  assert (test_eval_str 100 
            "sum <- 0;
             for (i = 1 to 11) {
               for (j = 1 to 11) {
                 sum <- sum + 1
               }
             };
             ret <- sum")
*)

(*==================================================================*)
(*==================================================================*)
(** Exo 3: Transformation de programmes *)

let rec use_for (p : 'a program) : bool =
  assert false (* TODO *)


let test_use_for (use : bool) (s : string) : bool =
  use_for (parse_prog s) = use

(*
let () =
  assert (test_use_for false "x <- 1; y <- 2; ret <- y")

let () =
  assert (test_use_for false
            "x <- 2; if x = 1 then ret <- 2 else ret <- 3")

let () = 
  assert (test_use_for false
            "x <- 10;
             r <- 0;
             while x > 0 {
               r <- x + r;
               x <- x-1
             };
             ret <- r")

let () = 
  assert (test_use_for true
            "fact <- 0;
             for (x = 0 to 6) {
               if x = 0 then
                 fact <- 1 
               else
                 fact <- x * fact
             };
             ret <- fact")
*)

(*------------------------------------------------------------------*)
let rec remove_for (p : 'a program) : 'a program =
  assert false (* TODO *)


(*------------------------------------------------------------------*)
let test_remove_for (p : 'a program) : bool =
  assert false (* TODO *)

let test_remove_for_str (s : string) : bool =
  test_remove_for (parse_prog s)

(*
let () =
  assert (test_eval_str 2 "ret <- 2 ")

let () =
  assert (test_remove_for_str "x <- 1; y <- 2; ret <- y")

let () =
  assert (test_remove_for_str "x <- 1; y <- 2; ret <- x + y")

let () =
  assert (test_remove_for_str
            "x <- 1; if x = 1 then ret <- 2 else ret <- 3")

let () =
  assert (test_remove_for_str
            "x <- 2; if x = 1 then ret <- 2 else ret <- 3")

let () = 
  assert (test_remove_for_str
            "x <- 10;
             r <- 0;
             while x > 0 {
               r <- x + r;
               x <- x-1
             };
             ret <- r")

let () = 
  assert (test_remove_for_str
            "fact <- 0;
             for (x = 0 to 6) {
               if x = 0 then
                 fact <- 1 
               else
                 fact <- x * fact
             };
             ret <- fact")

let () = 
  assert (test_remove_for_str
            "sum <- 0;
             for (i = 1 to 11) {
               for (j = 1 to 11) {
                 sum <- sum + 1
               }
             };
             ret <- sum")
*)

(*==================================================================*)
(*==================================================================*)
(** Exo 4: Optimization *)

let rec is_constant (e : 'a exparith) : bool =
  assert false (* TODO *)


(** Tests is_constant *)

let is_constant_str (s : string) = is_constant (parse_expr s)

(*
let () = assert (is_constant_str "5"                    )
let () = assert (is_constant_str "2 * 4"                )
let () = assert (is_constant_str "5 + 2 * 3 - 9"        )
let () = assert (is_constant_str "x"             = false)
let () = assert (is_constant_str "5 + x"         = false)
let () = assert (is_constant_str "x + 5"         = false)
let () = assert (is_constant_str "5 + x * 3 - 9" = false)
*)

(*------------------------------------------------------------------*)
let eval_constant (e : 'a exparith) : int = 
  assert false (* TODO *)


(** Tests eval_constant *)

let eval_constant_str (s : string) = eval_constant (parse_expr s)

(*
let () = assert (eval_constant_str "5"               = 5)
let () = assert (eval_constant_str "2 * 4"           = 8)
let () = assert (eval_constant_str "5 + (2 * 3) - 9" = 2)
let () = assert (eval_constant_str "5 + 2 * 3 - 9"   = 2)
*)

(*------------------------------------------------------------------*)
let rec subst_e (v : 'a) (x : int) (e : 'a exparith) : 'a exparith =
  assert false (* TODO *)


(** Tests subst_e *)

let subst_e_str (v : 'a) (x : int) (e : string) : 'a exparith =
  subst_e v x (parse_expr e)

(*
let () = assert (subst_e_str "x" 42 "x"             = parse_expr "42"             )
let () = assert (subst_e_str "x" 42 "5 + x"         = parse_expr "5 + 42"         )
let () = assert (subst_e_str "x" 42 "x + y"         = parse_expr "42 + y"         )
let () = assert (subst_e_str "x" 42 "z + x * x - y" = parse_expr "z + 42 * 42 - y")
*)

(*------------------------------------------------------------------*)
let rec subst_b (v : 'a) (x : int) (b : 'a expbool) : 'a expbool =
  assert false (* TODO *)


(** Tests subst_b *)

let subst_b_str (v : 'a) (x : int) (b : string) : 'a expbool =
  subst_b v x (parse_expr_b b)

(*
let () = assert (subst_b_str "x" 42 "y = x"         = parse_expr_b "y = 42"         )
let () = assert (subst_b_str "x" 42 "5 + x = y"     = parse_expr_b "5 + 42 = y"     )
let () = assert (subst_b_str "x" 42 "x + y = y + x" = parse_expr_b "42 + y = y + 42")
*)

(*------------------------------------------------------------------*)
let rec capture (v : 'a) (p : 'a program) : bool =
  assert false (* TODO *)


(** Tests capture *)

let capture_str (v : 'a) (p : string) : bool =
  capture v (parse_prog p)

(*
let () = assert (capture_str "x" "x <- 5"        )
let () = assert (capture_str "x" "y <- 5" = false)

let () = assert (capture_str "x" "y <- 5; x <- 42"        )
let () = assert (capture_str "x" "y <- 5; z <- 42" = false)

let () = assert (capture_str "x" "if (x < 10) then x <- 5 else z <- 10"        )
let () = assert (capture_str "x" "if (x < 10) then y <- 5 else x <- 10"        )
let () = assert (capture_str "x" "if (x < 10) then y <- 5 else z <- 10" = false)

let () = assert (capture_str "x" "y <- 0; for (x = 0 to 10) { y <- x * y }"        )
let () = assert (capture_str "x" "y <- 0; for (x = 0 to 10) { y <- 1 + y }"        )
let () = assert (capture_str "x" "y <- 0; for (i = 0 to 10) { y <- i * y }" = false)

let () = assert (capture_str "x" "while (y < z) { x <- i * y }"        )
let () = assert (capture_str "x" "while (y < z) { y <- i * y }" = false)
*)

(*------------------------------------------------------------------*)
let rec subst_p (v : 'a) (x : int) (p : 'a program) : 'a program =
  assert false (* TODO *)

and subst_p_list (v : 'a) (x : int) (p_l : 'a program list) : 'a program list =
  assert false (* TODO *)


(** Tests subst_p *)

let subst_p_str (v : 'a) (x : int) (p : string) : 'a program =
  subst_p v x (parse_prog p)

(*
let () = assert (subst_p_str "x" 7 "x <- 5" = parse_prog "x <- 5")
let () = assert (subst_p_str "x" 7 "x <- x" = parse_prog "x <- 7")
let () = assert (subst_p_str "x" 7 "y <- 5" = parse_prog "y <- 5")

let () = assert (subst_p_str "x" 7 "y <- x; x <- z" = parse_prog "y <- 7; x <- z")
let () = assert (subst_p_str "x" 7 "x <- z; y <- x" = parse_prog "x <- z; y <- x")

let () = assert
  (subst_p_str "x" 7 "{ if (y < 2) then y <- 5 else y <- 10 }; z <- x + y" =
   parse_prog        "{ if (y < 2) then y <- 5 else y <- 10 }; z <- 7 + y")
let () = assert
  (subst_p_str "x" 7 "{ if (y < 2) then x <- 5 else y <- 10 }; z <- x + y" =
   parse_prog        "{ if (y < 2) then x <- 5 else y <- 10 }; z <- x + y")
let () = assert
  (subst_p_str "x" 7 "{ if (y < 2) then y <- 5 else x <- 10 }; z <- x + y" =
   parse_prog        "{ if (y < 2) then y <- 5 else x <- 10 }; z <- x + y")

let () = assert (subst_p_str "x" 7 "y <- 5; for (i = 0 to 10) { y <- x * y }" =
                 parse_prog        "y <- 5; for (i = 0 to 10) { y <- 7 * y }")
let () = assert (subst_p_str "x" 7 "y <- 5; for (x = 0 to 10) { y <- x * y }" =
                 parse_prog        "y <- 5; for (x = 0 to 10) { y <- x * y }")

let () = 
  assert (subst_p_str "x" 7 "while (i < z) { y <- x * y; i <- i + 1 }" =
          parse_prog        "while (i < z) { y <- 7 * y; i <- i + 1  }")
let () = 
  assert (subst_p_str "x" 7 "while (y < z) { x <- i; y <- x * y; i <- i + 1  }" =
          parse_prog        "while (y < z) { x <- i; y <- x * y; i <- i + 1  }")
*)

(*------------------------------------------------------------------*)
let rec constant_prop (p : 'a program) : 'a program =
  assert false (* TODO *)

and constant_prop_l (p_l : 'a program list) : 'a program list =
  assert false (* TODO *)


(** Tests constant_prop *)

let constant_prop_str (p : string) : 'a program =
  constant_prop (parse_prog p)

(*
let () = assert (constant_prop_str "x <- 5" = parse_prog "x <- 5")

let () = assert (constant_prop_str "x <- 5; y <- x" =
                 parse_prog        "x <- 5; y <- 5")

let () = assert (constant_prop_str "x <- 5; y <- x; z <- x + y" = 
                 parse_prog        "x <- 5; y <- 5; z <- 10")
let () = assert (constant_prop_str "x <- 5; x <- 10; z <- 2 * x" = 
                 parse_prog        "x <- 5; x <- 10; z <- 20")

let () = assert
  (constant_prop_str "x <- 5; if (y < 2) then y <- x else y <- 10" =
   parse_prog        "x <- 5; if (y < 2) then y <- 5 else y <- 10")
let () = assert 
  (constant_prop_str "x <- 5; if (y < 2) then { x <- x * y; y <- x} else y <- 10" =
   parse_prog        "x <- 5; if (y < 2) then { x <- 5 * y; y <- x} else y <- 10")

let () = assert
  (constant_prop_str "x <- 5; while (i < z) { y <- x * y; i <- i + 1 }" =
   parse_prog        "x <- 5; while (i < z) { y <- 5 * y; i <- i + 1 }")
let () = assert
  (constant_prop_str "x <- 5; while (i < z) { y <- x * y; x <- i; i <- i + 1 }" =
   parse_prog        "x <- 5; while (i < z) { y <- x * y; x <- i; i <- i + 1 }")

let () = assert
  (constant_prop_str "y <- 0; x <- 5; for (i = 0 to 10) { y <- x * y }" =
   parse_prog        "y <- 0; x <- 5; for (i = 0 to 10) { y <- 5 * y }")
let () = assert
  (constant_prop_str "y <- 5; for (i = 0 to 10) { y <- x * y }" =
   parse_prog        "y <- 5; for (i = 0 to 10) { y <- x * y }")
*)
