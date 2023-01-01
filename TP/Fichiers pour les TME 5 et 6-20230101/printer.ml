open Syntax

(*---------------------------------------------------------------*)
(** expression pretty-printer *)
let rec pp_expr fmt : string exparith -> unit = function
  | Var x  -> Format.fprintf fmt "%s" x
  | Cste i -> Format.fprintf fmt "%d" i

  | Opp (Var  x) -> Format.fprintf fmt "- %s" x
  | Opp (Cste x) -> Format.fprintf fmt "- %d" x
  | Opp e  -> Format.fprintf fmt "- (%a)" pp_expr e

  | Plus (e1, e2) -> Format.fprintf fmt "@[(%a + %a)@]" pp_expr e1 pp_expr e2
  | Mult (e1, e2) -> Format.fprintf fmt "@[(%a * %a)@]" pp_expr e1 pp_expr e2
  | Div  (e1, e2) -> Format.fprintf fmt "@[(%a / %a)@]" pp_expr e1 pp_expr e2

(*---------------------------------------------------------------*)
(** comparison operators pretty-printer *)
let pp_comp_op2 fmt : comp_op2 -> unit = function
  | Eq  -> Format.fprintf fmt "="
  | Neq -> Format.fprintf fmt "<>"
  | Leq -> Format.fprintf fmt "<="
  | Lt  -> Format.fprintf fmt "<"
  | Geq -> Format.fprintf fmt ">="
  | Gt  -> Format.fprintf fmt ">"

(*---------------------------------------------------------------*)
(** boolean expression pretty-printer *)
let rec pp_expr_b fmt : string expbool -> unit = function
  | CompArith (cmp, e1, e2) ->
    Format.fprintf fmt "@[<hv 0>@[%a@] %a@ @[%a@]@]"
      pp_expr e1
      pp_comp_op2 cmp
      pp_expr e2

  | Not b -> Format.fprintf fmt "not (@[%a@])" pp_expr_b b

  | And (b1, b2) -> 
    Format.fprintf fmt "@[<hv 0>(@[%a@]) &&@ (@[%a@])@]"
      pp_expr_b b1 pp_expr_b b2

  | Or  (b1, b2) -> 
    Format.fprintf fmt "@[<hv 0>(@[%a@]) ||@ (@[%a@])@]" 
      pp_expr_b b1 pp_expr_b b2

(*---------------------------------------------------------------*)
(** program pretty-printer *)
let rec pp_prog fmt : string program -> unit = function
  | Assign (s, e) -> Format.fprintf fmt "@[%s <- @[%a@]@]" s pp_expr e

  | If (b, p1, p2) ->
    Format.fprintf fmt "@[<hv 0>\
                        if @[%a@] then {@;<1 2>\
                        @[%a@]@;\
                        } else {@;<1 2>\
                        @[%a@]@;\
                        }@]"
      pp_expr_b b
      pp_prog p1
      pp_prog p2

  | Seq l ->
    Format.fprintf fmt "@[<v 0>%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt -> Format.fprintf fmt "%a" pp_prog))
      l

  | While (b, p) ->
    Format.fprintf fmt "@[<v 0>\
                        while @[%a@] {@;<1 2>\
                        @[%a@]\
                        @;}@]"
      pp_expr_b b
      pp_prog p

  | For (x, i, j, p) ->
    Format.fprintf fmt "@[<v 0>\
                        for (%s = %d to %d){@;<1 2>\
                        @[%a@]\
                        @;}@]"
      x i j pp_prog p

(*---------------------------------------------------------------*)
(** AST printers *)
module AST = struct
  let rec pp_expr fmt : string exparith -> unit = function
    | Var x  -> Format.fprintf fmt "Var \"%s\"" x
    | Cste i -> Format.fprintf fmt "Cste %d" i
    | Opp e  -> Format.fprintf fmt "Opp (%a)" pp_expr e
    | Plus (e1, e2) -> Format.fprintf fmt "Plus (%a,%a)" pp_expr e1 pp_expr e2
    | Mult (e1, e2) -> Format.fprintf fmt "Mult (%a,%a)" pp_expr e1 pp_expr e2
    | Div  (e1, e2) -> Format.fprintf fmt  "Div (%a,%a)" pp_expr e1 pp_expr e2

  let pp_comp_op2 fmt : comp_op2 -> unit = function
    | Eq  -> Format.fprintf fmt "Eq"
    | Neq -> Format.fprintf fmt "Neq"
    | Leq -> Format.fprintf fmt "Leq"
    | Lt  -> Format.fprintf fmt "Lt"
    | Geq -> Format.fprintf fmt "Geq"
    | Gt  -> Format.fprintf fmt "Gt"

  let rec pp_expr_b fmt : string expbool -> unit = function
    | CompArith (cmp, e1, e2) ->
      Format.fprintf fmt "CompArith (%a, %a, %a)"
        pp_comp_op2 cmp
        pp_expr e1
        pp_expr e2

    | Not b -> Format.fprintf fmt "Not (%a)" pp_expr_b b

    | And (b1, b2) -> Format.fprintf fmt "And (%a, %a)" pp_expr_b b1 pp_expr_b b2
    | Or  (b1, b2) -> Format.fprintf fmt  "Or (%a, %a)" pp_expr_b b1 pp_expr_b b2


  let rec pp_prog fmt : string program -> unit = function
    | Assign (s, e) -> Format.fprintf fmt "@[Assign (\"%s\", %a)@]" s pp_expr e

    | If (b, p1, p2) ->
      Format.fprintf fmt "If (@[<v>%a,@;%a,@;%a@])"
        pp_expr_b b
        pp_prog p1
        pp_prog p2

    | Seq l ->
      Format.fprintf fmt "Seq [@[<v>%a@]]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@;")
           (fun fmt -> Format.fprintf fmt "(%a)" pp_prog))
        l

    | While (b, p) ->
      Format.fprintf fmt "@[While (@[<v>%a,@; %a@])@]"
        pp_expr_b b
        pp_prog p

    | For (x, i, j, p) ->
      Format.fprintf fmt "@[For (@[<v>\"%s\", %d, %d,@;%a@])@]"
        x i j pp_prog p
end
