let parse_file () =
  let argv = Sys.argv in
  let _ =
    if Array.length argv != 2 then (
      prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
      exit 1)
  in
  let ch = open_in argv.(1) in
  Javascript.Parser.program Javascript.Lexer.lexer (Lexing.from_channel ch)

let type_check_prog (p : Javascript.Ast.program) : Javascript.Ast.tipe =
  Js_typecheck.type_check_program p

let compile_prog (p : Javascript.Ast.program) : C.Ast.program =
  Js_compile.compile_program p

let dump (p : C.Ast.program) =
  let prog_str =
    "#include <stdio.h>\n\
     #include <stdlib.h>\n\n\
     union Value {\n\
    \    double num;\n\
    \    struct Closure* closurePtr;\n\
    \    union Value* var;\n\
     };\n\n\
     struct Variable {\n\
    \    union Value value;\n\
    \    struct Variable* next;\n\
     };\n\n\
     struct Environment {\n\
    \    struct Variable *variablesHead;\n\
     };\n\n\
     struct Closure {\n\
    \  union Value (*func)(struct Environment);\n\
    \  struct Environment env;\n\
     };\n\n" ^ C.Ast.string_of_program p
  in
  let _ = print_string prog_str in
  ()

let () =
  let js_program = parse_file () in
  let _ = type_check_prog js_program in
  (* let _ = print_string (Javascript.Ast.string_of_program js_program) in*)
  let c_program = compile_prog js_program in
  dump c_program
