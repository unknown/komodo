%{
open Ast
%}

%start program

/* nonterminals */
%type <Ast.program> program

/* terminals */
%token <int> INT
%token <string> ID
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA EQUAL
%token PLUS MINUS TIMES DIV
%token BANG
%token RETURN
%token LET CONST
%token FUNCTION
%token PRINT
%token EOF

%left PLUS MINUS
%left TIMES DIV
%left UNOP

%%

program:
  stmts EOF { $1 }

stmt:
  exp SEMI { Exp $1 }
  | RETURN exp SEMI { Return $2 }
  | LET ID EQUAL exp SEMI stmts { Decl (Let, $2, $4, $6) }
  | CONST ID EQUAL exp SEMI stmts { Decl (Const, $2, $4, $6) }
  | FUNCTION ID LPAREN params RPAREN LBRACE stmts RBRACE { Fn { name = $2; args = $4; body = $7 } }

stmts:
  stmt { $1 }
  | stmt stmts { Seq ($1, $2) }

param:
  ID { $1 }

params:
  param { [$1] }
  | param COMMA params { $1::$3 }

exp:
  ID { Var $1 }
  | INT { Int $1 }
  | unop exp %prec UNOP { Unop ($1, $2) }
  | exp binop exp { Binop ($2, $1, $3) }
  | exp EQUAL exp { Assign ($1, $3) }
  | LPAREN exp RPAREN { $2 }
  | exp LPAREN RPAREN { Call($1, []) }
  | exp LPAREN exps RPAREN { Call($1, $3) }
  | print { $1 }

exps:
  exp { [$1] }
  | exp COMMA exps { $1::$3 }

print:
  PRINT LPAREN exp RPAREN { Print $3 }

%inline unop:
  MINUS { UMinus }
  | BANG { Not }

%inline binop:
  PLUS { Plus }
  | MINUS { Minus }
  | TIMES { Times }
  | DIV { Div }
