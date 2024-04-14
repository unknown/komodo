exception Compile_error of string

let program : C.Ast.program ref = ref []

(* generate fresh labels *)
let label_counter = ref 0

let new_int () =
  label_counter := !label_counter + 1;
  !label_counter

(* generate a fresh temporary variable *)
let new_temp () = "T" ^ string_of_int (new_int ())

(* environment of a closure *)
type env = C.Ast.var list

(* get DeBruijn index of a variable *)
let lookup_arg (env : env) (x : C.Ast.var) : int =
  let rec lookup env' index =
    match env' with
    | [] -> raise (Compile_error ("Unbound value " ^ x))
    | hd :: tl -> if hd = x then index else lookup tl (index + 1)
  in
  lookup env 0

let get_arg (index : int) : C.Ast.exp =
  let rec helper index' acc : C.Ast.exp =
    match index' with
    | 0 -> Binop (Arrow, acc, Var "value")
    | _ -> helper (index' - 1) (Binop (Arrow, acc, Var "next"))
  in
  helper index (Var "env")

let malloc (typ : C.Ast.typ) : C.Ast.exp =
  Call (Var "malloc", [ Call (Var "sizeof", [ Var typ ]) ])

let free (e : C.Ast.exp) : C.Ast.stmt = Exp (Call (Var "free", [ e ]))

(* commonly used expressions *)
let result : C.Ast.exp = Var "result"
let result_num : C.Ast.exp = Binop (Dot, result, Var "num")
let result_closure_ptr : C.Ast.exp = Binop (Dot, result, Var "closurePtr")
let result_var : C.Ast.exp = Binop (Dot, result, Var "var")
let null : C.Ast.exp = Var "NULL"

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

(* replaces all guesses in `t` with their guessed types if they exist *)
let rec flatten_guesses (t : Javascript.Ast.tipe) : Javascript.Ast.tipe =
  match t with
  | Number_t | Bool_t | Unit_t | Tvar_t _ -> t
  | Fn_t (ts, tret) -> Fn_t (List.map flatten_guesses ts, flatten_guesses tret)
  | Guess_t tr -> ( match !tr with Some t' -> flatten_guesses t' | None -> t)

let tipe_of ((_, tr, _) : Javascript.Ast.exp) : Javascript.Ast.tipe =
  flatten_guesses !tr

let rec exp2stmt (e : Javascript.Ast.exp) (env : env) (left : bool) : C.Ast.stmt
    =
  let e', _, _ = e in
  match e' with
  | Number n -> Exp (Assign (result_num, Double n))
  | Var x ->
      let index = lookup_arg env x in
      let var : C.Ast.exp = Binop (Dot, get_arg index, Var "var") in
      if left then Exp (Assign (result_var, var))
      else Exp (Assign (result, Unop (Deref, var)))
  | ExpSeq (e1, e2) ->
      let s1 = exp2stmt e1 env left in
      let s2 = exp2stmt e2 env left in
      seq_stmts [ s1; s2 ]
  | Binop (op, e1, e2) ->
      (* TODO: check types of `e1` and `e2` *)
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
      Decl (("double", t), None, seq_stmts [ v1; store_v1; v2; store_result ])
  | Unop (op, e) ->
      let v = exp2stmt e env left in
      let store_result : C.Ast.stmt =
        Exp (Assign (result_num, Unop (unop2unop op, result_num)))
      in
      seq_stmts [ v; store_result ]
  | Assign (x, e) ->
      let t = new_temp () in
      let vx = exp2stmt x env true in
      let store_vx : C.Ast.stmt = Exp (Assign (Var t, result)) in
      let ve = exp2stmt e env left in
      let assign_var : C.Ast.stmt =
        Exp (Assign (Unop (Deref, Binop (Dot, Var t, Var "var")), result))
      in
      Decl (("union Value", t), None, seq_stmts [ vx; store_vx; ve; assign_var ])
  | Fn f ->
      let t1 = new_temp () in
      let t2 = new_temp () in
      let env' = List.fold_right (fun arg env -> arg :: env) f.args env in
      let _ =
        match f.name with
        | Some _ ->
            (* TODO: handle recursive functions *)
            stmt2fun f.body t1 env'
        | None -> stmt2fun f.body t1 env'
      in
      let malloc_closure : C.Ast.stmt =
        Exp (Assign (Var t2, malloc "struct Closure"))
      in
      let store_func : C.Ast.stmt =
        Exp (Assign (Binop (Arrow, Var t2, Var "func"), Var t1))
      in
      let store_env : C.Ast.stmt =
        Exp (Assign (Binop (Arrow, Var t2, Var "env"), Var "env"))
      in
      let store_result : C.Ast.stmt =
        Exp (Assign (result_closure_ptr, Var t2))
      in
      Decl
        ( ("struct Closure*", t2),
          Some null,
          seq_stmts [ malloc_closure; store_func; store_env; store_result ] )
  | Call (e, es) ->
      let t1 = new_temp () in
      let t2 = new_temp () in
      let rec compile_call (args : Javascript.Ast.exp list) (s : C.Ast.stmt) :
          C.Ast.stmt =
        match args with
        | [] -> s
        | e' :: tl ->
            let t3 = new_temp () in
            let t4 = new_temp () in

            let malloc_var : C.Ast.stmt =
              Exp (Assign (Var t3, malloc "struct Variable"))
            in

            let v = exp2stmt e' env left in
            let store_result : C.Ast.stmt = Exp (Assign (Var t4, result)) in
            let assign_var : C.Ast.stmt =
              Exp
                (Assign
                   ( Binop (Dot, Binop (Arrow, Var t3, Var "value"), Var "var"),
                     Unop (AddrOf, Var t4) ))
            in
            let store_next : C.Ast.stmt =
              Exp (Assign (Binop (Arrow, Var t3, Var "next"), Var t2))
            in
            let store_head : C.Ast.stmt = Exp (Assign (Var t2, Var t3)) in

            let rest : C.Ast.stmt = compile_call tl s in

            (* TODO: this segfaults if the call returns a closure referencing this var *)
            let free_var : C.Ast.stmt = free (Var t3) in

            Decl
              ( ("struct Variable*", t3),
                Some null,
                Decl
                  ( ("union Value", t4),
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
        Exp (Assign (Var t2, Binop (Arrow, Var t1, Var "env")))
      in
      let call_exp : C.Ast.exp =
        Call (Binop (Arrow, Var t1, Var "func"), [ Var t2 ])
      in
      let store_result : C.Ast.stmt = Exp (Assign (result, call_exp)) in
      let call_with_args = compile_call (List.rev es) store_result in

      Decl
        ( ("struct Closure*", t1),
          Some null,
          Decl
            ( ("struct Variable*", t2),
              Some null,
              seq_stmts [ v; store_closure; save_env; call_with_args ] ) )
  | Print e ->
      let v = exp2stmt e env left in
      let print : C.Ast.stmt =
        match tipe_of e with
        | Number_t ->
            Exp (Call (Var "printf", [ String "%.16g\\n"; result_num ]))
        | Bool_t ->
            Exp
              (Call
                 ( Var "printf",
                   [
                     String "%s\\n";
                     If (result_num, String "true", String "false");
                   ] ))
        | Unit_t -> Exp (Call (Var "printf", [ String "undefined\\n" ]))
        | t ->
            raise
              (Compile_error
                 ("Unsupported type " ^ Javascript.Ast.string_of_tipe t))
      in
      seq_stmts [ v; print ]

and stmt2stmt (s : Javascript.Ast.stmt) (env : env) : C.Ast.stmt =
  match s with
  | Exp e -> exp2stmt e env false
  | Seq (s1, s2) ->
      let s1' = stmt2stmt s1 env in
      let s2' = stmt2stmt s2 env in
      seq_stmts [ s1'; s2' ]
  | If (e, s1, s2) ->
      let t = new_temp () in
      let save_env : C.Ast.stmt = Exp (Assign (Var t, Var "env")) in
      let v = exp2stmt e env false in
      let s1' = stmt2stmt s1 env in
      let s2' = stmt2stmt s2 env in
      (* TODO: free all new declared variables from env *)
      let restore_env : C.Ast.stmt = Exp (Assign (Var "env", Var t)) in
      Decl
        ( ("struct Variable*", t),
          Some null,
          seq_stmts [ save_env; v; If (result_num, s1', s2'); restore_env ] )
  | While (e, s) ->
      let t1 = new_temp () in
      let t2 = new_temp () in
      let _ = stmt2fun (Javascript.Ast.Return e) t1 env in
      let save_env : C.Ast.stmt = Exp (Assign (Var t2, Var "env")) in
      let e' : C.Ast.exp = Binop (Dot, Call (Var t1, [ Var t2 ]), Var "num") in
      let s' = stmt2stmt s env in
      (* TODO: free all new declared variables from env *)
      let restore_env : C.Ast.stmt = Exp (Assign (Var "env", Var t2)) in
      Decl
        ( ("struct Variable*", t2),
          Some null,
          seq_stmts [ save_env; While (e', seq_stmts [ s'; restore_env ]) ] )
  | For (e1, e2, e3, s) ->
      stmt2stmt (Seq (Exp e1, While (e2, Seq (s, Exp e3)))) env
  | Return e ->
      let v = exp2stmt e env false in
      let return : C.Ast.stmt = Return (Some result) in
      seq_stmts [ v; return ]
  | Decl (_, x, e, s) ->
      let t1 = new_temp () in
      let t2 = new_temp () in

      let malloc_var : C.Ast.stmt =
        Exp (Assign (Var t1, malloc "struct Variable"))
      in

      let v = exp2stmt e env false in
      (* TODO: ponder deeply whether it is safe to store this on the stack *)
      let store_result : C.Ast.stmt = Exp (Assign (Var t2, result)) in

      let assign_var : C.Ast.stmt =
        Exp
          (Assign
             ( Binop (Dot, Binop (Arrow, Var t1, Var "value"), Var "var"),
               Unop (AddrOf, Var t2) ))
      in
      let store_next : C.Ast.stmt =
        Exp (Assign (Binop (Arrow, Var t1, Var "next"), Var "env"))
      in
      let store_head : C.Ast.stmt = Exp (Assign (Var "env", Var t1)) in

      let s' = stmt2stmt s (x :: env) in

      Decl
        ( ("struct Variable*", t1),
          Some null,
          Decl
            ( ("union Value", t2),
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
        ( ("struct Variable*", "env"),
          Some null,
          Decl (("union Value", "result"), None, s) )
    else Decl (("union Value", "result"), None, s)
  in

  let def = if name = "main" then ("int", "main") else ("union Value", name) in
  let args = if name = "main" then [] else [ ("struct Variable*", "env") ] in

  let s' = stmt2stmt s env in
  let return : C.Ast.stmt =
    if name = "main" then Return (Some (Int 0)) else Return (Some result)
  in
  let body = compile_body (seq_stmts [ s'; return ]) in

  let f : C.Ast.func = Fn { def; args; body } in
  program := f :: !program

let compile_program (p : Javascript.Ast.program) : C.Ast.program =
  let env = [] in
  let _ = stmt2fun p "main" env in
  List.rev !program
