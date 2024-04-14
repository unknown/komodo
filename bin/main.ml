type mode = Typecheck | Compile

let mode_of_string str = if str = "typecheck" then Typecheck else Compile

let parse_file () : mode * Javascript.Ast.program =
  let argv = Sys.argv in
  let _ =
    if
      Array.length argv != 3
      || not (argv.(1) = "typecheck" || argv.(1) = "compile")
    then (
      prerr_string
        ("usage: " ^ argv.(0) ^ " [typecheck | compile] [file-to-parse]\n");
      exit 1)
  in
  let ch = open_in argv.(2) in
  let mode = mode_of_string argv.(1) in
  let program =
    Javascript.Parser.program Javascript.Lexer.lexer (Lexing.from_channel ch)
  in
  (mode, program)

let type_check_prog (p : Javascript.Ast.program) : Javascript.Ast.tipe =
  Js_typecheck.type_check_program p

let compile_prog (p : Javascript.Ast.program) : C.Ast.program =
  let _ = type_check_prog p in
  Js_compile.compile_program p

let dump_program (p : C.Ast.program) =
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
     struct Closure {\n\
    \  union Value (*func)(struct Variable* env);\n\
    \  struct Variable* env;\n\
     };\n\n" ^ C.Ast.string_of_program p
  in
  print_string prog_str

let () =
  let mode, program = parse_file () in
  match mode with
  | Typecheck -> (
      try
        let _ = type_check_prog program in
        print_string (Javascript.Ast.string_of_program program)
      with Js_typecheck.TypeError e ->
        print_string e;
        print_newline ())
  | Compile ->
      let c_program = compile_prog program in
      dump_program c_program
