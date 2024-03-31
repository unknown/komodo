exception Compile_error of string

let program : C.Ast.program ref = ref []

(* generate fresh labels *)
let label_counter = ref 0

let new_int () =
  label_counter := !label_counter + 1;
  !label_counter

(* generate a fresh temporary variable *)
let new_temp () = "T" ^ string_of_int (new_int ())

let malloc (typ : C.Ast.typ) : C.Ast.exp =
  Call (Var "malloc", [ Call (Var "sizeof", [ Var typ ]) ])

let free (e : C.Ast.exp) : C.Ast.stmt = Exp (Call (Var "free", [ e ]))

(* helper variables to get values from result structs *)
let result_num : C.Ast.exp = Binop (Arrow, Var "result", Var "num")
let result_num_ptr : C.Ast.exp = Binop (Arrow, Var "result", Var "numPtr")

let result_closure_ptr : C.Ast.exp =
  Binop (Arrow, Var "result", Var "closurePtr")

(* helper functions to sequence statements *)
let seq_stmt (s1 : C.Ast.stmt) (s2 : C.Ast.stmt) : C.Ast.stmt = Seq (s1, s2)

let seq_stmts (stmts : C.Ast.stmt list) : C.Ast.stmt =
  match stmts with
  | [] -> raise (Compile_error "Empty statement list")
  | hd :: tl -> List.fold_left (fun acc stmt -> seq_stmt acc stmt) hd tl

let binop2binop (b : Javascript.Ast.binop) : C.Ast.binop =
  match b with Plus -> Plus | Minus -> Minus | Times -> Times | Div -> Div

let unop2unop (u : Javascript.Ast.unop) : C.Ast.unop =
  match u with Not -> Not | _ -> raise (Compile_error "Unsupported unop")

let rec exp2stmt (e : Javascript.Ast.exp) : C.Ast.stmt =
  match e with
  | Int i -> Exp (Assign (result_num, Int i))
  | Var x ->
      let assign_num : C.Ast.stmt = Exp (Assign (result_num, Var x)) in
      let assign_num_ptr : C.Ast.stmt =
        Exp (Assign (result_num_ptr, Unop (AddrOf, Var x)))
      in
      seq_stmts [ assign_num; assign_num_ptr ]
  | ExpSeq (e1, e2) -> seq_stmts [ exp2stmt e1; exp2stmt e2 ]
  | Binop (op, e1, e2) ->
      let t = new_temp () in
      let v1 = exp2stmt e1 in
      let store_v1 : C.Ast.stmt = Exp (Assign (Var t, result_num)) in
      let v2 = exp2stmt e2 in
      let eval : C.Ast.exp = Binop (binop2binop op, Var t, result_num) in
      let store_result : C.Ast.stmt = Exp (Assign (result_num, eval)) in
      Decl (("int", t), None, seq_stmts [ v1; store_v1; v2; store_result ])
  | Unop (op, e) ->
      let v = exp2stmt e in
      let eval : C.Ast.exp = Unop (unop2unop op, result_num) in
      let store_result : C.Ast.stmt = Exp (Assign (result_num, eval)) in
      seq_stmts [ v; store_result ]
  | Assign (x, e) ->
      let t = new_temp () in
      let vx = exp2stmt x in
      let store_vx : C.Ast.stmt = Exp (Assign (Var t, result_num_ptr)) in
      let ve = exp2stmt e in
      let assign : C.Ast.stmt =
        Exp (Assign (Unop (Deref, Var t), result_num))
      in
      Decl (("int*", t), Some (Int 0), seq_stmts [ vx; store_vx; ve; assign ])
  | Call (_, _) -> raise (Compile_error "Call not supported")
  | Print e ->
      let v = exp2stmt e in
      let print : C.Ast.stmt =
        Exp (Call (Var "printf", [ String "%d\\n"; result_num ]))
      in
      seq_stmts [ v; print ]

let rec stmt2stmt (s : Javascript.Ast.stmt) : C.Ast.stmt =
  match s with
  | Exp e -> exp2stmt e
  | Seq (s1, s2) -> Seq (stmt2stmt s1, stmt2stmt s2)
  | Fn _ -> raise (Compile_error "Fn not supported")
  | Return e ->
      let v = exp2stmt e in
      let return : C.Ast.stmt = Return (Var "result") in
      seq_stmts [ v; return ]
  | Decl (m, x, e, s) ->
      let typ = match m with Let -> "int" | Const -> "const int" in
      let v = exp2stmt e in
      let decl : C.Ast.stmt = Decl ((typ, x), Some result_num, stmt2stmt s) in
      seq_stmts [ v; decl ]

and stmt2fun (s : Javascript.Ast.stmt) (def : C.Ast.def) (args : C.Ast.def list)
    : unit =
  let _, name = def in
  let init (s : C.Ast.stmt) : C.Ast.stmt =
    if name = "main" then
      Decl (("struct Value*", "result"), Some (malloc "struct Value"), s)
    else Decl (("struct Value*", "result"), Some (malloc "struct Value"), s)
  in
  let return : C.Ast.stmt =
    if name = "main" then Return (Int 0) else Return (Var "result")
  in
  let body = init (Seq (stmt2stmt s, return)) in
  let f : C.Ast.func = Fn { def; args; body } in
  program := f :: !program

let compile_program (p : Javascript.Ast.program) : C.Ast.program =
  let args = [] in
  let _ = stmt2fun p ("int", "main") args in
  List.rev !program
