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

open Parsing;;
let _ = parse_error;;
# 2 "yaccParser.mly"
    open Syntax
    let mk_seq p1 p2 = 
      match p1, p2 with
      | Seq l, _ -> Seq (l @ [p2])
      | _, Seq l -> Seq (p1 :: l)
      | _ -> Seq [p1; p2]
# 41 "yaccParser.ml"
let yytransl_const = [|
  259 (* AND *);
  260 (* OR *);
  261 (* NOT *);
  262 (* NEQ *);
  263 (* EQ *);
  264 (* LT *);
  265 (* GT *);
  266 (* GEQ *);
  267 (* LEQ *);
  268 (* ASSIGN *);
  269 (* SEMICOLON *);
  270 (* LPAREN *);
  271 (* RPAREN *);
  272 (* LBRACKET *);
  273 (* RBRACKET *);
  274 (* MINUS *);
  275 (* PLUS *);
  276 (* TIMES *);
  277 (* DIV *);
  278 (* IF *);
  279 (* THEN *);
  280 (* ELSE *);
  281 (* FOR *);
  282 (* TO *);
  283 (* WHILE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\005\000\005\000\005\000\005\000\005\000\005\000\006\000\006\000\
\006\000\006\000\006\000\007\000\007\000\007\000\007\000\007\000\
\007\000\001\000\002\000\003\000\000\000\000\000\000\000"

let yylen = "\002\000\
\003\000\001\000\001\000\002\000\003\000\003\000\003\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\003\000\003\000\
\002\000\003\000\003\000\003\000\003\000\006\000\011\000\005\000\
\003\000\002\000\002\000\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\003\000\002\000\000\000\000\000\
\029\000\000\000\000\000\000\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\031\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\000\000\000\000\000\000\
\013\000\014\000\009\000\010\000\011\000\012\000\000\000\000\000\
\000\000\027\000\000\000\000\000\000\000\000\000\000\000\000\000\
\028\000\001\000\000\000\000\000\007\000\000\000\015\000\000\000\
\000\000\000\000\000\000\025\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\000\000\000\000\000\
\000\000\000\000\000\000\000\000\023\000"

let yydgoto = "\004\000\
\009\000\013\000\021\000\014\000\039\000\015\000\022\000"

let yysindex = "\115\000\
\041\255\052\255\003\255\000\000\000\000\000\000\041\255\041\255\
\000\000\093\000\052\255\052\255\000\000\104\255\101\000\250\254\
\003\255\052\255\001\255\052\255\000\000\002\000\061\255\000\255\
\041\255\041\255\041\255\041\255\000\000\046\255\088\255\060\255\
\000\000\000\000\000\000\000\000\000\000\000\000\041\255\052\255\
\052\255\000\000\041\255\252\254\004\255\027\255\008\255\003\255\
\000\000\000\000\000\255\000\255\000\000\066\255\000\000\070\255\
\046\255\046\255\070\255\000\000\003\255\031\255\003\255\034\255\
\246\254\039\255\005\255\003\255\045\255\000\000\034\255\064\255\
\054\255\056\255\003\255\043\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\073\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\045\000\000\000\067\000\000\000\083\000\
\078\000\087\000\068\000\000\000\000\000\000\000\000\000\004\000\
\000\000\000\000\000\000\000\000\000\000\000\000\026\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\009\000\000\000\021\000\239\255"

let yytablesize = 370
let yytable = "\044\000\
\004\000\049\000\048\000\020\000\016\000\043\000\040\000\041\000\
\048\000\010\000\040\000\041\000\060\000\068\000\046\000\023\000\
\024\000\048\000\017\000\027\000\031\000\070\000\005\000\063\000\
\018\000\022\000\061\000\019\000\062\000\020\000\064\000\030\000\
\032\000\051\000\052\000\053\000\054\000\066\000\045\000\069\000\
\047\000\005\000\006\000\065\000\006\000\067\000\048\000\056\000\
\040\000\041\000\071\000\059\000\005\000\006\000\007\000\048\000\
\011\000\076\000\008\000\077\000\057\000\058\000\040\000\041\000\
\073\000\012\000\008\000\021\000\074\000\008\000\072\000\075\000\
\017\000\000\000\055\000\050\000\000\000\018\000\025\000\026\000\
\027\000\028\000\016\000\025\000\026\000\027\000\019\000\025\000\
\026\000\027\000\028\000\000\000\029\000\033\000\034\000\035\000\
\036\000\037\000\038\000\000\000\042\000\000\000\050\000\000\000\
\000\000\025\000\026\000\027\000\028\000\033\000\034\000\035\000\
\036\000\037\000\038\000\001\000\002\000\003\000\000\000\000\000\
\000\000\025\000\026\000\027\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\004\000\000\000\004\000\004\000\
\004\000\004\000\004\000\004\000\000\000\004\000\048\000\004\000\
\004\000\004\000\004\000\004\000\020\000\004\000\000\000\004\000\
\004\000\005\000\005\000\020\000\005\000\005\000\005\000\005\000\
\005\000\005\000\000\000\005\000\000\000\005\000\005\000\005\000\
\005\000\005\000\022\000\005\000\000\000\005\000\005\000\006\000\
\006\000\022\000\006\000\006\000\006\000\006\000\006\000\006\000\
\000\000\006\000\000\000\006\000\006\000\006\000\006\000\006\000\
\000\000\006\000\000\000\006\000\006\000\008\000\008\000\000\000\
\008\000\008\000\008\000\008\000\008\000\008\000\000\000\008\000\
\021\000\008\000\008\000\008\000\021\000\016\000\016\000\017\000\
\017\000\008\000\008\000\021\000\018\000\018\000\000\000\017\000\
\000\000\016\000\016\000\000\000\018\000\019\000\019\000\040\000\
\041\000\016\000\000\000\000\000\000\000\019\000\025\000\026\000\
\027\000\028\000"

let yycheck = "\017\000\
\000\000\000\000\013\001\000\000\002\001\012\001\003\001\004\001\
\013\001\001\000\003\001\004\001\017\001\024\001\014\001\007\000\
\008\000\013\001\016\001\020\001\012\000\017\001\000\000\016\001\
\022\001\000\000\023\001\025\001\002\001\027\001\048\000\011\000\
\012\000\025\000\026\000\027\000\028\000\007\001\018\000\001\001\
\020\000\001\001\002\001\061\000\000\000\063\000\013\001\039\000\
\003\001\004\001\068\000\043\000\001\001\002\001\014\001\013\001\
\005\001\075\000\018\001\017\001\040\000\041\000\003\001\004\001\
\001\001\014\001\000\000\000\000\015\001\018\001\026\001\016\001\
\000\000\255\255\015\001\015\001\255\255\000\000\018\001\019\001\
\020\001\021\001\000\000\018\001\019\001\020\001\000\000\018\001\
\019\001\020\001\021\001\255\255\000\000\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\000\000\255\255\015\001\255\255\
\255\255\018\001\019\001\020\001\021\001\006\001\007\001\008\001\
\009\001\010\001\011\001\001\000\002\000\003\000\255\255\255\255\
\255\255\018\001\019\001\020\001\021\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\255\255\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\013\001\015\001\
\016\001\017\001\018\001\019\001\017\001\021\001\255\255\023\001\
\024\001\003\001\004\001\024\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\017\001\021\001\255\255\023\001\024\001\003\001\
\004\001\024\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\013\001\255\255\015\001\016\001\017\001\018\001\019\001\
\255\255\021\001\255\255\023\001\024\001\003\001\004\001\255\255\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\013\001\
\013\001\015\001\016\001\017\001\017\001\003\001\004\001\015\001\
\016\001\023\001\024\001\024\001\015\001\016\001\255\255\023\001\
\255\255\015\001\016\001\255\255\023\001\015\001\016\001\003\001\
\004\001\023\001\255\255\255\255\255\255\023\001\018\001\019\001\
\020\001\021\001"

let yynames_const = "\
  AND\000\
  OR\000\
  NOT\000\
  NEQ\000\
  EQ\000\
  LT\000\
  GT\000\
  GEQ\000\
  LEQ\000\
  ASSIGN\000\
  SEMICOLON\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  MINUS\000\
  PLUS\000\
  TIMES\000\
  DIV\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FOR\000\
  TO\000\
  WHILE\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 70 "yaccParser.mly"
                      ( _2 )
# 270 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "yaccParser.mly"
                      ( Var _1 )
# 277 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 72 "yaccParser.mly"
                      ( Cste _1 )
# 284 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "yaccParser.mly"
                      ( Opp _2 )
# 291 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "yaccParser.mly"
                  ( Plus (_1, Opp _3) )
# 299 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "yaccParser.mly"
                  ( Plus(_1,_3) )
# 307 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "yaccParser.mly"
                  ( Mult(_1,_3) )
# 315 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "yaccParser.mly"
                  ( Div (_1,_3) )
# 323 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "yaccParser.mly"
      ( Lt )
# 329 "yaccParser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "yaccParser.mly"
      ( Gt )
# 335 "yaccParser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "yaccParser.mly"
      ( Geq )
# 341 "yaccParser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "yaccParser.mly"
      ( Leq )
# 347 "yaccParser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "yaccParser.mly"
      ( Neq )
# 353 "yaccParser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "yaccParser.mly"
      ( Eq )
# 359 "yaccParser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_b) in
    Obj.repr(
# 88 "yaccParser.mly"
                          ( _2 )
# 366 "yaccParser.ml"
               : 'expr_b))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'comp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "yaccParser.mly"
                          ( CompArith (_2,_1,_3) )
# 375 "yaccParser.ml"
               : 'expr_b))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_b) in
    Obj.repr(
# 90 "yaccParser.mly"
                          ( Not _2 )
# 382 "yaccParser.ml"
               : 'expr_b))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_b) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_b) in
    Obj.repr(
# 91 "yaccParser.mly"
                          ( And (_1, _3) )
# 390 "yaccParser.ml"
               : 'expr_b))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_b) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_b) in
    Obj.repr(
# 92 "yaccParser.mly"
                          ( Or  (_1, _3) )
# 398 "yaccParser.ml"
               : 'expr_b))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'prog) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 96 "yaccParser.mly"
                                 ( mk_seq _1 _3 )
# 406 "yaccParser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "yaccParser.mly"
                                 ( Assign (_1,_3) )
# 414 "yaccParser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr_b) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'prog) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 98 "yaccParser.mly"
                                 ( If(_2,_4,_6) )
# 423 "yaccParser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 101 "yaccParser.mly"
                                 ( For(_3,_5,_7,_10) )
# 433 "yaccParser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr_b) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 104 "yaccParser.mly"
                                 ( While(_2,_4) )
# 441 "yaccParser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 106 "yaccParser.mly"
                                 ( _2 )
# 448 "yaccParser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 110 "yaccParser.mly"
           ( _1 )
# 455 "yaccParser.ml"
               : string Syntax.exparith))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_b) in
    Obj.repr(
# 113 "yaccParser.mly"
             ( _1 )
# 462 "yaccParser.ml"
               : string Syntax.expbool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 116 "yaccParser.mly"
           ( _1 )
# 469 "yaccParser.ml"
               : string Syntax.program))
(* Entry expr_top *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry expr_b_top *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry prog_top *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr_top (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : string Syntax.exparith)
let expr_b_top (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : string Syntax.expbool)
let prog_top (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : string Syntax.program)
