%{
open Ast
%}

%start program

/* nonterminals */
%type <Ast.program> program

/* terminals */
%token <int> INT
%token <string> ID
%token SEMI LPAREN RPAREN LBRACE RBRACE
%token PLUS MINUS TIMES DIV
%token BANG
%token EOF

%left PLUS MINUS
%left TIMES DIV
%left UNOP

%%

program:
  stmts EOF { $1 }

stmt:
  exp SEMI { Exp $1 }

stmts:
  stmt { $1 }
  | stmt stmts { Seq ($1, $2) }

exp:
  ID { Var $1 }
  | INT { Int $1 }
  | unop exp %prec UNOP { Unop ($1, $2) }
  | exp binop exp { Binop ($2, $1, $3) }
  | LPAREN exp RPAREN { $2 }

%inline unop:
  MINUS { UMinus }
  | BANG { Not }

%inline binop:
  PLUS { Plus }
  | MINUS { Minus }
  | TIMES { Times }
  | DIV { Div }
