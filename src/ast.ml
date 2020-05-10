 (* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mul | Div | Mod | Equal | Neq | Less | And | Or

type typ = Int | Float | Bool | String | Array of typ * int | Any | List

type expr =
    Literal of int
  | FloatLit of float
  | BoolLit of bool
  | Id of string
  | ArrayLit of expr list
  | ArrayAccess of string * int
  | StringLit of string
  | Binop of expr * op * expr
  | Assign of assign
  | ArrayAssign of string * int * expr
  (* function call *)
  | Call of string * expr list

and assign = string * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  (* return *)
  | Return of expr
  | LocalVarDef of var_def

and def = 
  | VarDef of var_def
  | FuncDef of func_def
(* int x: name binding *)
and var_def = 
  | Decl of typ * string
  | Init of typ * assign
(* func_def: ret_typ fname formals locals body *)
and func_def = {
  rtyp: typ;
  fname: string;
  formals: var_def list;
  body: stmt list;
}

type program = def list

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
  | Or -> "||"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | FloatLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | ArrayLit(l) -> "{" ^ (String.concat ", " (List.map string_of_expr l)) ^ "}"
  | Id(s) -> s
  | ArrayAccess(var, idx) -> var ^ "[" ^ (string_of_int idx) ^ "]"
  | StringLit(s) -> "\"" ^ s ^ "\""
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(assign) -> string_of_assign assign
  | ArrayAssign(v, i, e) -> v ^ "[" ^ (string_of_int i) ^ "]" ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
and
string_of_assign (string, expr) = string ^ " "^ string_of_expr expr

let rec string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | String -> "string"
  | Array(t, len) -> (string_of_typ t) ^ "[" ^ (string_of_int len) ^ "]"
  | Any -> "any"
  | List -> "list"

let string_of_vdecl = function
    Decl(t, id) -> string_of_typ t ^ " " ^ id ^ ";\n"
  | Init (t, assign) -> string_of_typ t  ^ " " ^ string_of_assign assign ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | LocalVarDef(l) -> string_of_vdecl l

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals ) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_def = function
    VarDef(var_def) -> string_of_vdecl var_def
  | FuncDef(func_def) -> string_of_fdecl func_def

let string_of_program (defs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_def defs) ^ "\n"

  let extract_var = function
      Decl (typ, string) -> (typ, string)
    | Init(typ, assign) -> (typ, fst assign)