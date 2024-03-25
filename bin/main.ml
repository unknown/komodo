let parse_file () =
  let argv = Sys.argv in
  let _ =
    if Array.length argv != 2 then (
      prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
      exit 1)
  in
  let ch = open_in argv.(1) in
  Javascript.Parser.program Javascript.Lexer.lexer (Lexing.from_channel ch)

let () =
  let prog = parse_file () in
  let prog_string = Javascript.Ast.string_of_program prog in
  print_string prog_string
