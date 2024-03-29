exception Compile_error of string

let program : C.Ast.program ref = ref []

let binop2binop (b : Javascript.Ast.binop) : C.Ast.binop =
  match b with Plus -> Plus | Minus -> Minus | Times -> Times | Div -> Div

let unop2unop (u : Javascript.Ast.unop) : C.Ast.unop =
  match u with Not -> Not | _ -> raise (Compile_error "Unsupported unop")

let rec exp2exp (e : Javascript.Ast.exp) : C.Ast.exp =
  match e with
  | Int i -> Int i
  | Var x -> Var x
  | Binop (op, e1, e2) -> Binop (binop2binop op, exp2exp e1, exp2exp e2)
  | Unop (op, e) -> Unop (unop2unop op, exp2exp e)
  | Assign (x, e) -> Assign (exp2exp x, exp2exp e)
  | Call (e, es) -> Call (exp2exp e, List.map exp2exp es)
  | Print e -> Call (Var "printf", [ String "%d\\n"; exp2exp e ])

let rec stmt2stmt (s : Javascript.Ast.stmt) : C.Ast.stmt =
  match s with
  | Exp e -> Exp (exp2exp e)
  | Seq (s1, s2) -> Seq (stmt2stmt s1, stmt2stmt s2)
  | Fn f ->
      let args = List.map (fun arg -> ("int", arg)) f.args in
      let _ = stmt2fun f.body ("int", f.name) args in
      C.Ast.skip
  | Return e -> Return (exp2exp e)
  | Decl (m, x, e, s) ->
      let typ = match m with Let -> "int" | Const -> "const int" in
      Decl ((typ, x), exp2exp e, stmt2stmt s)

and stmt2fun (s : Javascript.Ast.stmt) (def : C.Ast.def) (args : C.Ast.def list)
    : unit =
  let body : C.Ast.stmt = Seq (stmt2stmt s, Return (Int 0)) in
  let f : C.Ast.func = Fn { def; args; body } in
  program := f :: !program

let compile_program (p : Javascript.Ast.program) : C.Ast.program =
  let _ = stmt2fun p ("int", "main") [] in
  List.rev !program
