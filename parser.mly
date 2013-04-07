%{
open Exp
%}

%token <int> INT
%token <bool> BOOL
%token <string> VARIABLE
%token LPAREN
%token RPAREN
%token EQUAL
%token LESS
%token MINUS_GREATER
%token PLUS
%token MINUS
%token AST
%token EOI
%token THEN
%token ELSE
%token LET
%token FUN
%token REC
%token IN
%token IF
%token LBRACKET_RBRACKET
%token COLON_COLON
%token MATCH
%token WITH
%token BAR

%nonassoc IN
%nonassoc LET
%nonassoc THEN
%nonassoc ELSE
%right COLON_COLON
%nonassoc LESS
%left PLUS MINUS
%left AST
%left prec_app

%type <Exp.t> toplevel
%start toplevel

%%

toplevel:
  | exp EOI
      { $1 }

exp:
  | simple_exp
      { $1 }
  | exp PLUS exp
      { Plus ($1, $3) }
  | exp MINUS exp
      { Minus ($1, $3) }
  | exp AST exp
      { Times ($1, $3) }
  | exp LESS exp
      { Lt ($1, $3) }
  | IF exp THEN exp ELSE exp
      { If ($2, $4, $6) }
  | LET VARIABLE EQUAL exp IN exp
      { Let ($2, $4, $6) }
  | FUN VARIABLE MINUS_GREATER exp
      { Fun ($2, $4) }
  | exp simple_exp 
      %prec prec_app
      { App ($1, $2) }
  | LET REC VARIABLE EQUAL exp IN exp
      { LetRec ($3, $5, $7) }
  | exp COLON_COLON exp
      { Cons ($1, $3) }
  | MATCH exp WITH LBRACKET_RBRACKET MINUS_GREATER exp BAR VARIABLE COLON_COLON VARIABLE MINUS_GREATER exp
      { Match ($2, $6, $8, $10, $12) }

simple_exp:
  | INT
      { Int ($1) }
  | BOOL
      { Bool ($1) }
  | VARIABLE
      { Var ($1) }
  | LPAREN exp RPAREN
      { $2 }
  | LBRACKET_RBRACKET
       { Nil }
