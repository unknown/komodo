{
open Parser
open Lexing

exception Syntax_error of string

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

(* definition section *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let cr='\013'
let nl='\010'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let int = ('+'|'-')? digit+
let id = alpha (alpha|digit|'_')*

(* rules section *)
rule lexer = parse
  | ';' { SEMI }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ',' { COMMA }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '!' { BANG }
  | "function" { FUNCTION }
  | "console.log" { PRINT }
  | eol { incr_lineno lexbuf; lexer lexbuf }
  | ws+ { lexer lexbuf }
  | int { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | "/*" { comment lexbuf }
  | eof { EOF }
  | _ { raise (Syntax_error ("Invalid character: " ^ Lexing.lexeme lexbuf)) }

and comment = parse
  | eol { incr_lineno lexbuf; comment lexbuf }
  | eof { raise (Syntax_error "Unterminated comment") }
  | "*/" { lexer lexbuf }
  | _ { comment lexbuf }
