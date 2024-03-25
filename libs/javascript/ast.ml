type var = string
type binop = Plus | Minus | Times | Div
type unop = UMinus | Not

type exp =
  | Int of int
  | Var of var
  | Binop of binop * exp * exp
  | Unop of unop * exp
  | Call of exp * exp list

type funcsig = { name : var; args : var list; body : stmt }
and stmt = Exp of exp | Seq of stmt * stmt | Fn of funcsig

type program = stmt

let string_of_binop (op : binop) : string =
  match op with Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/"

let string_of_unop (op : unop) : string =
  match op with UMinus -> "-" | Not -> "!"

let rec string_of_exp (e : exp) : string =
  match e with
  | Int i -> string_of_int i
  | Var x -> x
  | Binop (op, e1, e2) ->
      "(" ^ string_of_exp e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_exp e2
      ^ ")"
  | Unop (op, e) -> string_of_unop op ^ string_of_exp e
  | Call (e, es) -> string_of_exp e ^ "(" ^ string_of_exps es ^ ")"

and string_of_exps (es : exp list) =
  match es with
  | [] -> ""
  | e :: [] -> string_of_exp e
  | e :: es -> string_of_exp e ^ "," ^ string_of_exps es

let rec string_of_stmt (s : stmt) : string =
  match s with
  | Exp e -> string_of_exp e ^ ";\n"
  | Seq (s1, s2) -> string_of_stmt s1 ^ string_of_stmt s2
  | Fn f ->
      "function " ^ f.name ^ "(" ^ String.concat "," f.args ^ ") {\n"
      ^ string_of_stmt f.body ^ "}\n"

let string_of_program (p : program) : string = string_of_stmt p
