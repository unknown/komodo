type var = string
type binop = Plus | Minus | Times | Div
type unop = UMinus | Not

type exp =
  | Int of int
  | Var of var
  | Binop of binop * exp * exp
  | Unop of unop * exp

type stmt = Exp of exp | Seq of stmt * stmt
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

let rec string_of_stmt (s : stmt) : string =
  match s with
  | Exp e -> string_of_exp e ^ ";\n"
  | Seq (s1, s2) -> string_of_stmt s1 ^ string_of_stmt s2

let string_of_program (p : program) : string = string_of_stmt p
