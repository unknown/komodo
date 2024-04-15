open Javascript.Ast

exception TypeError of string

let type_error (s : string) ((_, _, pos) : exp) =
  raise (TypeError (string_of_pos pos ^ ":\nError: " ^ s))

let minus (lst1 : 'a list) (lst2 : 'a list) : 'a list =
  List.filter (fun x -> not (List.memq x lst2)) lst1

let union (lst1 : 'a list) (lst2 : 'a list) : 'a list = lst1 @ minus lst2 lst1
let guess () = Guess_t (ref None)

type env = (var * tipe_scheme) list

let extend (env : env) (x : var) (s : tipe_scheme) : env = (x, s) :: env

let rec lookup (env : env) (x : var) : tipe_scheme option =
  match env with
  | [] -> None
  | (x', s) :: tl -> if x = x' then Some s else lookup tl x

let var_counter = ref 0

let fresh_var () : var =
  let curr_counter = !var_counter in
  var_counter := curr_counter + 1;
  "v" ^ string_of_int curr_counter

(* returns whether the reference `tr` already occurs in type `t` *)
let rec occurs (tr : tipe option ref) (t : tipe) : bool =
  match t with
  | Number_t | Bool_t | Unit_t | Tvar_t _ -> false
  | Fn_t (ts, tret) -> List.exists (occurs tr) ts || occurs tr tret
  | Guess_t tr' -> (
      if tr == tr' then true
      else match !tr' with Some t' -> occurs tr t' | None -> false)

(* returns whether `t1` and `t2` are equal *)
let rec tipes_equal (t1 : tipe) (t2 : tipe) : bool =
  match (t1, t2) with
  | Number_t, Number_t | Bool_t, Bool_t | Unit_t, Unit_t | Tvar_t _, Tvar_t _ ->
      t1 = t2
  | Fn_t (ts1, tret1), Fn_t (ts2, tret2) ->
      List.length ts1 = List.length ts2
      && List.for_all2 tipes_equal ts1 ts2
      && tipes_equal tret1 tret2
  | Guess_t tr1, Guess_t tr2 -> (
      match (!tr1, !tr2) with
      | Some t1', Some t2' -> tipes_equal t1' t2'
      | Some t1', None -> occurs tr1 t1'
      | None, Some t2' -> occurs tr2 t2'
      | None, None -> tr1 == tr2)
  | Guess_t tr1, t2 -> (
      match !tr1 with Some t1' -> tipes_equal t1' t2 | None -> false)
  | t1, Guess_t _ -> tipes_equal t2 t1
  | _ -> false

(* tries to unify `t1` can be unified with `t2` and returns whether it succeeded *)
let rec unify (t1 : tipe) (t2 : tipe) : bool =
  if tipes_equal t1 t2 then true
  else
    match (t1, t2) with
    | Fn_t (ts1, tret1), Fn_t (ts2, tret2) ->
        List.length ts1 = List.length ts2
        && List.for_all2 unify ts1 ts2
        && unify tret1 tret2
    | Guess_t tr1, t2 -> (
        match !tr1 with
        | Some t1' -> unify t1' t2
        | None ->
            if occurs tr1 t2 then
              raise
                (TypeError
                   ("The type variable " ^ string_of_tipe t1 ^ " occurs inside "
                  ^ string_of_tipe t2))
            else (
              tr1 := Some t2;
              true))
    | t1, Guess_t _ -> unify t2 t1
    | _ -> false

(* substitutes all type variables in `t` with corresponding type variables in `vs` *)
let rec substitute (vs : (tvar * tipe) list) (t : tipe) : tipe =
  match t with
  | Number_t | Bool_t | Unit_t -> t
  | Tvar_t tvar ->
      let _, t' = List.find (fun (tvar', _) -> tvar == tvar') vs in
      t'
  | Fn_t (ts, tret) -> Fn_t (List.map (substitute vs) ts, substitute vs tret)
  | Guess_t tr -> (
      match !tr with
      | Some t' ->
          let t'' = substitute vs t' in
          Guess_t (ref (Some t''))
      | None -> t)

(* takes a type scheme `s` and returns its type representation *)
let instantiate (s : tipe_scheme) : tipe =
  match s with
  | Forall (vs, t) ->
      let b = List.map (fun a -> (a, guess ())) vs in
      substitute b t

(* returns all the guesses in a type *)
let rec guesses_of_tipe (t : tipe) : tipe list =
  match t with
  | Number_t | Bool_t | Unit_t | Tvar_t _ -> []
  | Fn_t (ts, tret) ->
      let ts_guesses = List.fold_left union [] (List.map guesses_of_tipe ts) in
      let tret_guesses = guesses_of_tipe tret in
      union ts_guesses tret_guesses
  | Guess_t tr -> (
      match !tr with Some t' -> guesses_of_tipe t' | None -> [ t ])

(* returns all the guesses in a type scheme *)
let guesses_of (s : tipe_scheme) : tipe list =
  match s with Forall (_, t) -> guesses_of_tipe t

(* substitutes all guesses in `t` with their corresponding type variables *)
let rec subst_guesses (gs_vs : (tipe * tvar) list) (t : tipe) : tipe =
  match t with
  | Number_t | Bool_t | Unit_t | Tvar_t _ -> t
  | Fn_t (ts, tret) ->
      Fn_t (List.map (subst_guesses gs_vs) ts, subst_guesses gs_vs tret)
  | Guess_t tr -> (
      match List.find_opt (fun (t', _) -> t == t') gs_vs with
      | Some (_, tvar) -> Tvar_t tvar
      | None -> (
          match !tr with
          | Some t' ->
              let t'' = subst_guesses gs_vs t' in
              tr := Some t'';
              t
          | None -> t))

(* takes an environment `env` and a type `t` and returns a type scheme *)
let generalize (env : env) (t : tipe) : tipe_scheme =
  let t_gs = guesses_of_tipe t in
  let env_list_gs = List.map (fun (_, s) -> guesses_of s) env in
  let env_gs = List.fold_left union [] env_list_gs in
  let diff = minus t_gs env_gs in
  let gs_vs = List.map (fun g -> (g, fresh_var ())) diff in
  let tc = subst_guesses gs_vs t in
  Forall (List.map snd gs_vs, tc)

let type_error_string (t1 : tipe) (t2 : tipe) : string =
  "This expression has type " ^ string_of_tipe t1
  ^ " but an expression was expected of type " ^ string_of_tipe t2

let rec type_check_exp (env : env) (e : exp) : tipe =
  let e', tr, _ = e in
  let t =
    match e' with
    | Number _ -> Number_t
    | Var x -> (
        match lookup env x with
        | Some s -> instantiate s
        | None -> type_error ("Unbound value " ^ x) e)
    | ExpSeq (_, _) -> type_error "TODO" e
    | Binop (op, e1, e2) -> (
        let t1 = type_check_exp env e1 in
        let t2 = type_check_exp env e2 in
        match op with
        | Plus | Minus | Times | Div ->
            if unify t1 Number_t then
              if unify t2 Number_t then Number_t
              else type_error (type_error_string t2 Number_t) e2
            else type_error (type_error_string t1 Number_t) e1
        | Eq | Neq ->
            if unify t1 t2 then Bool_t
            else type_error (type_error_string t2 t1) e2
        | Lt | Lte | Gt | Gte ->
            if unify t1 Number_t then
              if unify t2 Number_t then Bool_t
              else type_error (type_error_string t2 Number_t) e2
            else type_error (type_error_string t1 Number_t) e1
        | And | Or ->
            (* TODO: the resulting type should be `t1 | t2` *)
            if unify t1 t2 then t1 else type_error (type_error_string t2 t1) e2)
    | Unop (op, e') -> (
        let t = type_check_exp env e' in
        match op with
        | UMinus ->
            if unify t Number_t then Number_t
            else type_error (type_error_string t Number_t) e
        | Not -> Bool_t)
    | Assign (e1, e2) ->
        let t1 = type_check_exp env e1 in
        let t2 = type_check_exp env e2 in
        if unify t1 t2 then t1 else type_error (type_error_string t2 t1) e2
    | Fn f ->
        let ts = List.map (fun _ -> guess ()) f.args in
        let env' =
          List.fold_left2
            (fun env' x t -> extend env' x (Forall ([], t)))
            env f.args ts
        in
        (* TODO: `tb` should be a union of all the returns *)
        let g = guess () in
        let env' = extend env' f.name (Forall ([], Fn_t (ts, g))) in
        let tb = type_check_stmt env' f.body in
        if unify g tb then Fn_t (ts, tb)
        else type_error (type_error_string g tb) e
    | Call (e', es) ->
        let t = type_check_exp env e' in
        let ts = List.map (type_check_exp env) es in
        let g = guess () in
        if unify t (Fn_t (ts, g)) then g
        else type_error (type_error_string t (Fn_t (ts, g))) e
    | Print e' ->
        let _ = type_check_exp env e' in
        Unit_t
  in
  tr := t;
  !tr

and type_check_stmt (env : env) (s : stmt) : tipe =
  match s with
  | Exp e -> type_check_exp env e
  | Seq (s1, s2) ->
      let _ = type_check_stmt env s1 in
      let t2 = type_check_stmt env s2 in
      t2
  | If (e, s1, s2) ->
      let _ = type_check_exp env e in
      let t1 = type_check_stmt env s1 in
      let t2 = type_check_stmt env s2 in
      (* TODO: the resulting type should be `t1 | t2` *)
      if unify t1 t2 then t1 else raise (TypeError (type_error_string t2 t1))
  | While (e, s) ->
      let _ = type_check_exp env e in
      let _ = type_check_stmt env s in
      Unit_t
  | For (e1, e2, e3, s) ->
      let _ = type_check_exp env e1 in
      let _ = type_check_exp env e2 in
      let _ = type_check_exp env e3 in
      let _ = type_check_stmt env s in
      Unit_t
  | Return e -> type_check_exp env e
  | Decl (_, x, e, s) ->
      let ts = generalize env (type_check_exp env e) in
      type_check_stmt (extend env x ts) s

let type_check_program (p : program) = type_check_stmt [] p
