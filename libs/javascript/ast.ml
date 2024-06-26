type var = string
type tvar = string
type mut = Let | Const
type pos = Lexing.position

type tipe =
  | Number_t
  | Object_t of (var * tipe) list
  | Bool_t
  | Unit_t
  | Tvar_t of tvar
  | Fn_t of tipe list * tipe
  | Guess_t of tipe option ref

type tipe_scheme = Forall of tvar list * tipe

type binop =
  | Plus
  | Minus
  | Times
  | Div
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  | And
  | Or

type unop = UMinus | Not | ObjProp of var

type funcsig = { name : var; args : var list; body : stmt }

and rexp =
  | Number of float
  | Object of (var * exp) list
  | Var of var
  | ExpSeq of exp * exp
  | Binop of binop * exp * exp
  | Unop of unop * exp
  | Assign of exp * exp
  | Fn of funcsig
  | Call of exp * exp list
  | Print of exp

and exp = rexp * tipe ref * pos

and stmt =
  | Exp of exp
  | Seq of stmt * stmt
  | If of exp * stmt * stmt
  | While of exp * stmt
  | For of exp * exp * exp * stmt
  | Return of exp
  | Decl of mut * var * exp * stmt

(* simulate a skip statement *)
let skip : stmt = Exp (Number 0., ref Number_t, Lexing.dummy_pos)

type program = stmt

let tvar_counter = ref 0

let fresh_tvar () : string =
  let curr_counter = !tvar_counter in
  tvar_counter := curr_counter + 1;
  "ptvar" ^ string_of_int curr_counter

type env = (tipe option ref * tipe) list

let env : env ref = ref []

let lookup (tr : tipe option ref) : tipe =
  let rec helper (env' : env) (r : tipe option ref) : tipe =
    match env' with
    | [] ->
        let v = Tvar_t (fresh_tvar ()) in
        env := (r, v) :: !env;
        v
    | (r', t) :: tl -> if r' == r then t else helper tl r
  in
  helper !env tr

let string_of_pos (pos : pos) =
  "Line " ^ string_of_int pos.pos_lnum ^ ", character "
  ^ string_of_int (pos.pos_cnum - pos.pos_bol + 1)

let rec string_of_tipe (t : tipe) : string =
  match t with
  | Number_t -> "number"
  | Object_t ps ->
      let ps_str =
        String.concat " "
          (List.map (fun (x, t) -> x ^ ": " ^ string_of_tipe t ^ ";") ps)
      in
      "{ " ^ ps_str ^ " }"
  | Bool_t -> "boolean"
  | Unit_t -> "void"
  | Tvar_t tvar -> "'" ^ tvar
  | Fn_t (ts, tret) ->
      "("
      ^ String.concat ", " (List.map string_of_tipe ts)
      ^ ") => (" ^ string_of_tipe tret ^ ")"
  | Guess_t tr -> (
      match !tr with
      | Some t -> string_of_tipe t
      | None -> string_of_tipe (lookup tr))

let string_of_binop (op : binop) : string =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Eq -> "==="
  | Neq -> "!=="
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_unop (op : unop) : string =
  match op with UMinus -> "-" | Not -> "!" | ObjProp prop -> "." ^ prop

let string_of_mut (m : mut) : string =
  match m with Let -> "let" | Const -> "const"

let rec string_of_exp ((e, _, _) : exp) (level : int) : string =
  match e with
  | Number n -> string_of_float n
  | Object ps ->
      let tabs = String.make (level * 2) ' ' in
      let p_tabs = String.make ((level + 1) * 2) ' ' in
      let ps_str =
        String.concat "\n"
          (List.map
             (fun (x, e) ->
               p_tabs ^ x ^ ": " ^ string_of_exp e (level + 1) ^ ";")
             ps)
      in
      "{\n" ^ ps_str ^ "\n" ^ tabs ^ "}"
  | Var x -> x
  | ExpSeq (e1, e2) -> string_of_exp e1 level ^ ", " ^ string_of_exp e2 level
  | Binop (op, e1, e2) ->
      "(" ^ string_of_exp e1 level ^ " " ^ string_of_binop op ^ " "
      ^ string_of_exp e2 level ^ ")"
  | Unop (op, e) -> (
      match op with
      | ObjProp _ -> string_of_exp e level ^ string_of_unop op
      | _ -> string_of_unop op ^ string_of_exp e level)
  | Assign (x, e) -> string_of_exp x level ^ " = " ^ string_of_exp e level
  | Fn f ->
      let tabs = String.make (level * 2) ' ' in
      let args = String.concat ", " f.args in
      "function" ^ f.name ^ "(" ^ args ^ ") {\n"
      ^ string_of_stmt f.body (level + 1)
      ^ tabs ^ "}"
  | Call (e, es) ->
      let args =
        String.concat ", " (List.map (fun e' -> string_of_exp e' level) es)
      in
      string_of_exp e level ^ "(" ^ args ^ ")"
  | Print e -> "console.log(" ^ string_of_exp e level ^ ")"

and string_of_stmt (s : stmt) (level : int) : string =
  let tabs = String.make (level * 2) ' ' in
  match s with
  | Exp e -> tabs ^ string_of_exp e level ^ ";\n"
  | Seq (s1, s2) -> string_of_stmt s1 level ^ string_of_stmt s2 level
  | If (e, s1, s2) ->
      tabs ^ "if (" ^ string_of_exp e level ^ ") {\n"
      ^ string_of_stmt s1 (level + 1)
      ^ tabs ^ "}\n" ^ tabs ^ "else {\n"
      ^ string_of_stmt s2 (level + 1)
      ^ tabs ^ "}\n"
  | While (e, s) ->
      tabs ^ "while (" ^ string_of_exp e level ^ ") {\n"
      ^ string_of_stmt s (level + 1)
      ^ tabs ^ "}\n"
  | For (e1, e2, e3, s) ->
      tabs ^ "for (" ^ string_of_exp e1 level ^ "; " ^ string_of_exp e2 level
      ^ "; " ^ string_of_exp e3 level ^ ") {\n"
      ^ string_of_stmt s (level + 1)
      ^ tabs ^ "}\n"
  | Return e -> tabs ^ "return " ^ string_of_exp e level ^ ";\n"
  | Decl (m, x, e, s) ->
      let _, tr, _ = e in
      let tx = ": " ^ string_of_tipe !tr in
      tabs ^ string_of_mut m ^ " " ^ x ^ tx ^ " = " ^ string_of_exp e level
      ^ ";\n" ^ string_of_stmt s level

let string_of_program (p : program) : string = string_of_stmt p 0
