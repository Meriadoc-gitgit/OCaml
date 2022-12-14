{
  open Lexing
  open YaccParser

  let newline lexbuf =
    let p = lexbuf.Lexing.lex_curr_p in
    let q =
      { p with Lexing.
        pos_lnum = p.Lexing.pos_lnum+1 ;
        pos_bol = p.Lexing.pos_cnum }
    in
      lexbuf.Lexing.lex_curr_p <- q

  exception Lexical_error of string

  let unterminated_comment () =
    raise (Lexical_error "unterminated comment")

}

let name = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let int = ['0'-'9'] ['0'-'9']*

rule token = parse
| [' ' '\t']          { token lexbuf }
| '\n'                { newline lexbuf ; token lexbuf }
| "(*"                { comment lexbuf; token lexbuf }

| "&&"                { AND }
| "||"                { OR }
| "not"               { NOT }

| '='                 { EQ }
| "<>"                { NEQ }
| '<'                 { LT }
| '>'                 { GT }
| ">="                { GEQ }
| "<="                { LEQ }

| "<-"                { ASSIGN }
| ';'                 { SEMICOLON }

| '('                 { LPAREN }
| ')'                 { RPAREN }

| '{'                 { LBRACKET }
| '}'                 { RBRACKET }

| '-'                 { MINUS }
| '+'                 { PLUS }
| '*'                 { TIMES }
| '/'                 { DIV }

| "if"                { IF }
| "then"              { THEN }
| "else"              { ELSE }
| "for"               { FOR }
| "to"                { TO }
| "while"             { WHILE }

| name as n           { ID n }
| int as i            { INT (int_of_string i) }

| eof                 { EOF }

and comment = parse
  | "*)"        { () }
  | "(*"        { comment lexbuf; comment lexbuf }
  | "\n"        { new_line lexbuf; comment lexbuf }
  | eof         { unterminated_comment () }
  | _           { comment lexbuf }
