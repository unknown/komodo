%{
open Ast

let guess () = ref (Guess_t (ref None))
%}

%start program

/* nonterminals */
%type <Ast.program> program

/* terminals */
%token <int> INT
%token <string> ID
%token SEMI LPAREN RPAREN LBRACE RBRACE LANGLE RANGLE COMMA EQUAL
%token PLUS MINUS TIMES DIV AND OR
%token BANG
%token RETURN IF ELSE WHILE FOR
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
  | LBRACE stmts RBRACE { $2 }
  | LET ID EQUAL exp SEMI stmts { Decl (Let, $2, $4, $6) }
  | IF LPAREN exp RPAREN stmt ELSE stmt { If ($3, $5, $7) }
  | IF LPAREN exp RPAREN stmt { If ($3, $5, skip) }
  | WHILE LPAREN exp RPAREN stmt { While ($3, $5) }
  // | FOR LPAREN exp SEMI exp SEMI exp RPAREN stmt { For ($3, $5, $7, $9) }
  | CONST ID EQUAL exp SEMI stmts { Decl (Const, $2, $4, $6) }

stmts:
  stmt { $1 }
  | stmt stmts { Seq ($1, $2) }

param:
  ID { $1 }

params:
  param { [$1] }
  | param COMMA params { $1::$3 }

exp:
  ID { (Var $1, guess ()) }
  | INT { (Number (float_of_int $1), guess ()) }
  // | exp COMMA exp { ExpSeq($1, $3) }
  | unop exp %prec UNOP { (Unop ($1, $2), guess ()) }
  | exp binop exp { (Binop ($2, $1, $3), guess ()) }
  | exp EQUAL exp { (Assign ($1, $3), guess ()) }
  | LPAREN exp RPAREN { $2 }
  | FUNCTION LPAREN RPAREN LBRACE stmts RBRACE { (Fn { name = None; args = []; body = $5 }, guess ()) }
  | FUNCTION LPAREN params RPAREN LBRACE stmts RBRACE { (Fn { name = None; args = $3; body = $6 }, guess ()) }
  | FUNCTION ID LPAREN RPAREN LBRACE stmts RBRACE { (Fn { name = Some($2); args = []; body = $6 }, guess ()) }
  | FUNCTION ID LPAREN params RPAREN LBRACE stmts RBRACE { (Fn { name = Some($2); args = $4; body = $7 }, guess ()) }
  | exp LPAREN RPAREN { (Call($1, []), guess ()) }
  | exp LPAREN exps RPAREN { (Call($1, $3), guess ()) }
  | print { ($1, guess ()) }

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
  | EQUAL EQUAL EQUAL { Eq }
  | BANG EQUAL EQUAL { Neq }
  | LANGLE { Lt }
  | LANGLE EQUAL { Lte }
  | RANGLE { Gt }
  | RANGLE EQUAL { Gte }
  | AND AND { And }
  | OR OR { Or }
