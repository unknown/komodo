type var = string
type mut = Let | Const
type binop = Plus | Minus | Times | Div
type unop = UMinus | Not

type exp =
  | Int of int
  | Var of var
  | ExpSeq of exp * exp
  | Binop of binop * exp * exp
  | Unop of unop * exp
  | Assign of exp * exp
  | Call of exp * exp list
  | Print of exp

type funcsig = { name : var; args : var list; body : stmt }

and stmt =
  | Exp of exp
  | Seq of stmt * stmt
  | Fn of funcsig
  | Return of exp
  | Decl of mut * var * exp * stmt

type program = stmt

let string_of_binop (op : binop) : string =
  match op with Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/"

let string_of_unop (op : unop) : string =
  match op with UMinus -> "-" | Not -> "!"

let rec string_of_exp (e : exp) : string =
  match e with
  | Int i -> string_of_int i
  | Var x -> x
  | ExpSeq (e1, e2) -> string_of_exp e1 ^ ", " ^ string_of_exp e2
  | Binop (op, e1, e2) ->
      "(" ^ string_of_exp e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_exp e2
      ^ ")"
  | Unop (op, e) -> string_of_unop op ^ string_of_exp e
  | Assign (x, e) -> string_of_exp x ^ " = " ^ string_of_exp e
  | Call (e, es) -> string_of_exp e ^ "(" ^ string_of_exps es ^ ")"
  | Print e -> "console.log(" ^ string_of_exp e ^ ")"

and string_of_exps (es : exp list) =
  String.concat ", " (List.map string_of_exp es)

let string_of_mut (m : mut) : string =
  match m with Let -> "let" | Const -> "const"

let rec string_of_stmt (s : stmt) (level : int) : string =
  let tabs = String.make (level * 2) ' ' in
  match s with
  | Exp e -> tabs ^ string_of_exp e ^ ";\n"
  | Seq (s1, s2) -> string_of_stmt s1 level ^ string_of_stmt s2 level
  | Fn f ->
      tabs ^ "function " ^ f.name ^ "(" ^ String.concat ", " f.args ^ ") {\n"
      ^ string_of_stmt f.body (level + 1)
      ^ tabs ^ "}\n"
  | Return e -> tabs ^ "return " ^ string_of_exp e ^ ";\n"
  | Decl (m, x, e, s) ->
      tabs ^ string_of_mut m ^ " " ^ x ^ " = " ^ string_of_exp e ^ ";\n"
      ^ string_of_stmt s level

let string_of_program (p : program) : string = string_of_stmt p 0
