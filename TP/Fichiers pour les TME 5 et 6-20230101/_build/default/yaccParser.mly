%{
    open Syntax
    let mk_seq p1 p2 = 
      match p1, p2 with
      | Seq l, _ -> Seq (l @ [p2])
      | _, Seq l -> Seq (p1 :: l)
      | _ -> Seq [p1; p2]
%}

%token <int> INT
%token <string> ID

%token AND 
%token OR 
%token NOT 

%token NEQ 
%token EQ 
%token LT 
%token GT 
%token GEQ 
%token LEQ 

%token ASSIGN 
%token SEMICOLON 

%token LPAREN 
%token RPAREN 

%token LBRACKET 
%token RBRACKET 

%token MINUS 
%token PLUS 
%token TIMES 
%token DIV 

%token IF 
%token THEN 
%token ELSE 
%token FOR 
%token TO 
%token WHILE 

%token EOF 

/* precedences */

%nonassoc NOT
%right AND OR

%nonassoc DIV
%left PLUS MINUS 
%left TIMES

%nonassoc ELSE
%right SEMICOLON 

%start expr_top
%start expr_b_top
%start prog_top

%type <string Syntax.exparith> expr_top
%type <string Syntax.expbool>  expr_b_top
%type <string Syntax.program>  prog_top

%%

expr:
| LPAREN expr RPAREN  { $2 }
| ID                  { Var $1 }
| INT                 { Cste $1 }
| MINUS expr          { Opp $2 }
| expr MINUS expr { Plus ($1, Opp $3) }
| expr PLUS  expr { Plus($1,$3) }
| expr TIMES expr { Mult($1,$3) }
| expr DIV   expr { Div ($1,$3) }

comp:
| LT  { Lt }
| GT  { Gt }
| GEQ { Geq }
| LEQ { Leq }
| NEQ { Neq }
| EQ  { Eq }

expr_b:
| LPAREN expr_b RPAREN    { $2 }
| expr comp expr          { CompArith ($2,$1,$3) }
| NOT expr_b              { Not $2 }
| expr_b AND expr_b       { And ($1, $3) }
| expr_b OR  expr_b       { Or  ($1, $3) }

prog:
| prog SEMICOLON prog
                                 { mk_seq $1 $3 }
| ID ASSIGN expr                 { Assign ($1,$3) }
| IF expr_b THEN prog ELSE prog  { If($2,$4,$6) }

| FOR LPAREN ID EQ INT TO INT RPAREN LBRACKET prog RBRACKET
                                 { For($3,$5,$7,$10) }

| WHILE expr_b LBRACKET prog RBRACKET
                                 { While($2,$4) }

| LBRACKET prog RBRACKET         { $2 }


expr_top:
| expr EOF { $1 }

expr_b_top:
| expr_b EOF { $1 }

prog_top:
| prog EOF { $1 }
