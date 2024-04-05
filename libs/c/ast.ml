type typ = string
type var = string
type def = typ * var

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
  | Arrow
  | Dot

type unop = Not | Deref | AddrOf | Cast of typ

type exp =
  | Int of int
  | String of string
  | Var of var
  | ExpSeq of exp * exp
  | Binop of binop * exp * exp
  | Unop of unop * exp
  | Assign of exp * exp
  | Call of exp * exp list

type stmt =
  | Exp of exp
  | Seq of stmt * stmt
  | If of exp * stmt * stmt
  | While of exp * stmt
  | For of exp * exp * exp * stmt
  | Return of exp option
  | Decl of def * exp option * stmt

type funcsig = { def : def; args : def list; body : stmt }
type func = Fn of funcsig

let skip : stmt = Exp (Int 0) (* simulate a skip statement *)

type program = func list

let string_of_def ((t, x) : def) : string = t ^ " " ^ x

let string_of_binop (b : binop) : string =
  match b with
  | Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Div -> " / "
  | Eq -> " == "
  | Neq -> " != "
  | Lt -> " < "
  | Lte -> " <= "
  | Gt -> " > "
  | Gte -> " >= "
  | And -> " && "
  | Or -> " || "
  | Arrow -> "->"
  | Dot -> "."

let string_of_unop (u : unop) : string =
  match u with
  | Not -> "!"
  | Deref -> "*"
  | AddrOf -> "&"
  | Cast t -> "(" ^ t ^ ") "

let rec string_of_exp (e : exp) : string =
  match e with
  | Int i -> string_of_int i
  | String s -> "\"" ^ s ^ "\""
  | Var x -> x
  | ExpSeq (e1, e2) -> string_of_exp e1 ^ ", " ^ string_of_exp e2
  | Binop (op, e1, e2) ->
      "(" ^ string_of_exp e1 ^ string_of_binop op ^ string_of_exp e2 ^ ")"
  | Unop (op, e) -> string_of_unop op ^ string_of_exp e
  | Assign (x, e) -> string_of_exp x ^ " = " ^ string_of_exp e
  | Call (e, es) -> string_of_exp e ^ "(" ^ string_of_exps es ^ ")"

and string_of_exps (es : exp list) =
  String.concat ", " (List.map string_of_exp es)

let rec string_of_stmt (s : stmt) (level : int) : string =
  let tabs = String.make (level * 4) ' ' in
  match s with
  | Exp e -> tabs ^ string_of_exp e ^ ";\n"
  | Seq (e1, e2) -> string_of_stmt e1 level ^ string_of_stmt e2 level
  | If (e, s1, s2) ->
      tabs ^ "if (" ^ string_of_exp e ^ ") {\n"
      ^ string_of_stmt s1 (level + 1)
      ^ tabs ^ "}\n" ^ tabs ^ "else {\n"
      ^ string_of_stmt s2 (level + 1)
      ^ tabs ^ "}\n"
  | While (e, s) ->
      tabs ^ "while (" ^ string_of_exp e ^ ") {\n"
      ^ string_of_stmt s (level + 1)
      ^ tabs ^ "}\n"
  | For (e1, e2, e3, s) ->
      tabs ^ "for (" ^ string_of_exp e1 ^ "; " ^ string_of_exp e2 ^ "; "
      ^ string_of_exp e3 ^ ") {\n"
      ^ string_of_stmt s (level + 1)
      ^ tabs ^ "}\n"
  | Return e -> (
      match e with
      | Some e -> tabs ^ "return " ^ string_of_exp e ^ ";\n"
      | None -> tabs ^ "return;\n")
  | Decl (d, e, s) ->
      let v = match e with Some e -> " = " ^ string_of_exp e | None -> "" in
      tabs ^ string_of_def d ^ v ^ ";\n" ^ string_of_stmt s level

let string_of_func (fn : func) : string =
  let (Fn f) = fn in
  string_of_def f.def ^ "("
  ^ String.concat ", " (List.map (fun d -> string_of_def d) f.args)
  ^ ") {\n" ^ string_of_stmt f.body 1 ^ "}\n"

let string_of_program (p : program) : string =
  String.concat "\n" (List.map string_of_func p)
