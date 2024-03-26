let parse_file () =
  let argv = Sys.argv in
  let _ =
    if Array.length argv != 2 then (
      prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
      exit 1)
  in
  let ch = open_in argv.(1) in
  Javascript.Parser.program Javascript.Lexer.lexer (Lexing.from_channel ch)

let compile_prog (p : Javascript.Ast.program) : C.Ast.program =
  Js_compile.compile_program p

let dump (p : C.Ast.program) =
  let prog_str = C.Ast.string_of_program p in
  let _ = print_string prog_str in
  ()

let () =
  let js_program = parse_file () in
  (* let _ = print_string (Javascript.Ast.string_of_program js_program) in*)
  let c_program = compile_prog js_program in
  dump c_program
