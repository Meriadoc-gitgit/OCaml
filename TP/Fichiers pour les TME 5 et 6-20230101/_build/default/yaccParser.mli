type token =
  | INT of (int)
  | ID of (string)
  | AND
  | OR
  | NOT
  | NEQ
  | EQ
  | LT
  | GT
  | GEQ
  | LEQ
  | ASSIGN
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | MINUS
  | PLUS
  | TIMES
  | DIV
  | IF
  | THEN
  | ELSE
  | FOR
  | TO
  | WHILE
  | EOF

val expr_top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string Syntax.exparith
val expr_b_top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string Syntax.expbool
val prog_top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string Syntax.program
