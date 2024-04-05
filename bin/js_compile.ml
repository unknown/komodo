exception Compile_error of string

type program = { code : C.Ast.program; globals : C.Ast.def list }

let program : program ref = ref { code = []; globals = [] }

(* generate fresh labels *)
let label_counter = ref 0

let new_int () =
  label_counter := !label_counter + 1;
  !label_counter

(* generate a fresh temporary variable *)
let new_temp () = "T" ^ string_of_int (new_int ())

(* TODO: also store local variables *)
(* environment of a closure *)
type env = C.Ast.var list

(* get DeBruijn index of a variable *)
let lookup_arg (env : env) (x : C.Ast.var) : int option =
  let rec lookup env' index =
    match env' with
    | [] -> None
    | hd :: tl -> if hd = x then Some index else lookup tl (index + 1)
  in
  lookup env 0

let get_arg (index : int) : C.Ast.exp =
  let rec helper index' acc : C.Ast.exp =
    match index' with
    | 0 -> Binop (Arrow, acc, Var "value")
    | _ -> helper (index' - 1) (Binop (Arrow, acc, Var "next"))
  in
  helper index (Binop (Arrow, Var "env", Var "variablesHead"))

let lookup_closure (x : C.Ast.var) : C.Ast.exp option =
  let rec helper (globals : C.Ast.def list) : C.Ast.exp option =
    match globals with
    | [] -> None
    | (_, y) :: tl -> if x = y then Some (Var x) else helper tl
  in
  helper !program.globals

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
  match b with
  | Plus -> Plus
  | Minus -> Minus
  | Times -> Times
  | Div -> Div
  | Eq -> Eq
  | Neq -> Neq
  | Lt -> Lt
  | Lte -> Lte
  | Gt -> Gt
  | Gte -> Gte
  | And -> And
  | Or -> Or

let unop2unop (u : Javascript.Ast.unop) : C.Ast.unop =
  match u with Not -> Not | _ -> raise (Compile_error "Unsupported unop")

let rec exp2stmt (e : Javascript.Ast.exp) (env : env) (left : bool) : C.Ast.stmt
    =
  match e with
  | Int i -> Exp (Assign (result_num, Int i))
  | Var x -> (
      match lookup_arg env x with
      | Some index ->
          if left then
            Exp
              (Assign (result_num_ptr, Binop (Dot, get_arg index, Var "numPtr")))
          else
            Exp
              (Assign
                 ( result_num,
                   Unop (Deref, Binop (Dot, get_arg index, Var "numPtr")) ))
      | None -> (
          match lookup_closure x with
          | Some closure ->
              Exp (Assign (result_closure_ptr, Unop (AddrOf, closure)))
          | None -> raise (Compile_error ("Unbound variable: " ^ x))))
  | ExpSeq (e1, e2) ->
      let s1 = exp2stmt e1 env left in
      let s2 = exp2stmt e2 env left in
      seq_stmts [ s1; s2 ]
  | Binop (op, e1, e2) ->
      let t = new_temp () in
      let v1 = exp2stmt e1 env left in
      let store_v1 : C.Ast.stmt = Exp (Assign (Var t, result_num)) in
      let v2 = exp2stmt e2 env left in
      let store_result : C.Ast.stmt =
        match op with
        | And ->
            If
              ( Var t,
                Exp (Assign (result_num, result_num)),
                Exp (Assign (result_num, Var t)) )
        | Or ->
            If
              ( Var t,
                Exp (Assign (result_num, Var t)),
                Exp (Assign (result_num, result_num)) )
        | _ ->
            Exp (Assign (result_num, Binop (binop2binop op, Var t, result_num)))
      in
      Decl (("int", t), None, seq_stmts [ v1; store_v1; v2; store_result ])
  | Unop (op, e) ->
      let v = exp2stmt e env left in
      let store_result : C.Ast.stmt =
        Exp (Assign (result_num, Unop (unop2unop op, result_num)))
      in
      seq_stmts [ v; store_result ]
  | Assign (x, e) ->
      let t = new_temp () in
      let vx = exp2stmt x env true in
      let store_vx : C.Ast.stmt = Exp (Assign (Var t, result_num_ptr)) in
      let ve = exp2stmt e env left in
      let assign : C.Ast.stmt =
        Exp (Assign (Unop (Deref, Var t), result_num))
      in
      Decl (("int*", t), Some (Int 0), seq_stmts [ vx; store_vx; ve; assign ])
  | Call (e, es) ->
      let t1 = new_temp () in
      let t2 = new_temp () in
      let rec compile_call (args : Javascript.Ast.exp list) (s : C.Ast.stmt) :
          C.Ast.stmt =
        match args with
        | [] -> s
        | hd :: tl ->
            let typ = "int" in
            let t3 = new_temp () in
            let t4 = new_temp () in

            let malloc_var : C.Ast.stmt =
              Exp (Assign (Var t3, malloc "struct Variable"))
            in

            let v = exp2stmt hd env left in
            let store_result : C.Ast.stmt = Exp (Assign (Var t4, result_num)) in

            let assign_var : C.Ast.stmt =
              Exp
                (Assign
                   ( Binop
                       (Dot, Binop (Arrow, Var t3, Var "value"), Var "numPtr"),
                     Unop (AddrOf, Var t4) ))
            in
            let store_next : C.Ast.stmt =
              Exp
                (Assign
                   ( Binop (Arrow, Var t3, Var "next"),
                     Binop (Arrow, Var t2, Var "variablesHead") ))
            in
            let store_head : C.Ast.stmt =
              Exp (Assign (Binop (Arrow, Var t2, Var "variablesHead"), Var t3))
            in

            let rest : C.Ast.stmt = compile_call tl s in

            let free_var : C.Ast.stmt = free (Var t3) in

            Decl
              ( ("struct Variable*", t3),
                Some (Int 0),
                Decl
                  ( (typ, t4),
                    None,
                    seq_stmts
                      [
                        malloc_var;
                        v;
                        store_result;
                        assign_var;
                        store_next;
                        store_head;
                        rest;
                        free_var;
                      ] ) )
      in
      let v = exp2stmt e env left in
      let store_closure : C.Ast.stmt =
        Exp (Assign (Var t1, result_closure_ptr))
      in
      let save_env : C.Ast.stmt =
        Exp
          (Assign
             ( Binop (Arrow, Var t2, Var "variablesHead"),
               Binop
                 (Arrow, Binop (Arrow, Var t1, Var "env"), Var "variablesHead")
             ))
      in
      let call : C.Ast.stmt =
        Exp (Call (Binop (Arrow, Var t1, Var "func"), [ Var t2; Var "result" ]))
      in
      let call_with_args = compile_call (List.rev es) call in
      let free_env : C.Ast.stmt = free (Var t2) in
      Decl
        ( ("struct Closure*", t1),
          Some (Int 0),
          Decl
            ( ("struct Environment*", t2),
              Some (malloc "struct Environment"),
              seq_stmts [ v; store_closure; save_env; call_with_args; free_env ]
            ) )
  | Print e ->
      let v = exp2stmt e env left in
      let print : C.Ast.stmt =
        Exp (Call (Var "printf", [ String "%d\\n"; result_num ]))
      in
      seq_stmts [ v; print ]

let rec stmt2stmt (s : Javascript.Ast.stmt) (env : env) : C.Ast.stmt =
  match s with
  | Exp e -> exp2stmt e env false
  | Seq (s1, s2) ->
      let s1' = stmt2stmt s1 env in
      let s2' = stmt2stmt s2 env in
      seq_stmts [ s1'; s2' ]
  | If (e, s1, s2) ->
      let v = exp2stmt e env false in
      let s1' = stmt2stmt s1 env in
      let s2' = stmt2stmt s2 env in
      seq_stmts [ v; If (Binop (Neq, result_num, Int 0), s1', s2') ]
  | While (e, s) ->
      let t1 = new_temp () in
      let _ = stmt2fun (Javascript.Ast.Return e) t1 env in
      let e' : C.Ast.exp =
        ExpSeq (Call (Var t1, [ Var "env"; Var "result" ]), result_num)
      in
      let s' = stmt2stmt s env in
      While (e', s')
  | For (e1, e2, e3, s) ->
      stmt2stmt (Seq (Exp e1, While (e2, Seq (s, Exp e3)))) env
  | Fn f ->
      program :=
        {
          code = !program.code;
          globals = ("struct Closure", f.name) :: !program.globals;
        };
      let t = new_temp () in
      let env' = List.fold_right (fun arg env -> arg :: env) f.args env in
      let _ = stmt2fun f.body t env' in
      let store_func : C.Ast.stmt =
        Exp (Assign (Binop (Dot, Var f.name, Var "func"), Var t))
      in
      let store_env : C.Ast.stmt =
        Exp (Assign (Binop (Dot, Var f.name, Var "env"), Var "env"))
      in
      seq_stmts [ store_func; store_env ]
  | Return e ->
      let v = exp2stmt e env false in
      let return : C.Ast.stmt = Return None in
      seq_stmts [ v; return ]
  | Decl (_, x, e, s) ->
      let typ = "int" in
      let t1 = new_temp () in
      let t2 = new_temp () in

      let malloc_var : C.Ast.stmt =
        Exp (Assign (Var t1, malloc "struct Variable"))
      in

      let v = exp2stmt e env false in
      let store_result : C.Ast.stmt = Exp (Assign (Var t2, result_num)) in

      let assign_var : C.Ast.stmt =
        Exp
          (Assign
             ( Binop (Dot, Binop (Arrow, Var t1, Var "value"), Var "numPtr"),
               Unop (AddrOf, Var t2) ))
      in
      let store_next : C.Ast.stmt =
        Exp
          (Assign
             ( Binop (Arrow, Var t1, Var "next"),
               Binop (Arrow, Var "env", Var "variablesHead") ))
      in
      let store_head : C.Ast.stmt =
        Exp (Assign (Binop (Arrow, Var "env", Var "variablesHead"), Var t1))
      in

      let s' = stmt2stmt s (x :: env) in

      Decl
        ( ("struct Variable*", t1),
          Some (Int 0),
          Decl
            ( (typ, t2),
              None,
              seq_stmts
                [
                  malloc_var;
                  v;
                  store_result;
                  assign_var;
                  store_next;
                  store_head;
                  s';
                ] ) )

and stmt2fun (s : Javascript.Ast.stmt) (name : C.Ast.var) (env : env) : unit =
  let compile_body (s : C.Ast.stmt) : C.Ast.stmt =
    if name = "main" then
      Decl
        ( ("struct Environment*", "env"),
          Some (malloc "struct Environment"),
          Decl (("union Value*", "result"), Some (malloc "union Value"), s) )
    else s
  in

  let def = if name = "main" then ("int", "main") else ("void", name) in
  let args =
    if name = "main" then []
    else [ ("struct Environment*", "env"); ("union Value*", "result") ]
  in

  let s' = stmt2stmt s env in
  let return : C.Ast.stmt =
    if name = "main" then Return (Some (Int 0)) else Return None
  in
  let body = compile_body (seq_stmts [ s'; return ]) in

  let f : C.Ast.func = Fn { def; args; body } in
  program := { code = f :: !program.code; globals = !program.globals }

let compile_program (p : Javascript.Ast.program) : program =
  let env = [] in
  let _ = stmt2fun p "main" env in
  { code = List.rev !program.code; globals = !program.globals }
