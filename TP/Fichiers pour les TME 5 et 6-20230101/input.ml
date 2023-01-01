open Syntax

module Parser = YaccParser

(*------------------------------------------------------------------*)
let parse_expr (s : string) : string exparith =
  Parser.expr_top Lexer.token (Lexing.from_string s)

let parse_expr_b (s : string) : string expbool =
  Parser.expr_b_top Lexer.token (Lexing.from_string s)

let parse_prog (s : string) : string program =
  Parser.prog_top Lexer.token (Lexing.from_string s)
