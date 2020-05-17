(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx 
and sx =
    SLiteral of int
  | SFloatLit of float
  | SBoolLit of bool
  | SArrayLit of sexpr list
  | SId of string
  | SArrayAccess of string * int
  | SStringLit of string
  | SBinop of sexpr * op * sexpr
  | SAssign of sassign
  | SArrayAssign of string * int * sexpr
  (* call *)
  | SCall of string * sexpr list
  | SFuncExpr of sfunc_def
  | SClosure of sclsr_expr
  and sclsr_expr = {
    ind: int;
    fvs: bind list;
}

and sassign = string * sexpr

and sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  (* return *)
  | SReturn of sexpr
  | SLocalVarDef of svar_def
and sdef = 
  | SVarDef of svar_def
  | SFuncDef of sfunc_def
and svar_def = 
  | SDecl of typ * string
  | SInit of typ * sassign
(* func_def: ret_typ fname formals body *)
and sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: svar_def list;
  sbody: sstmt list;
}

type sprogram = sdef list

(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLiteral(l) -> string_of_int l
      | SFloatLit(l) -> string_of_float l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SArrayLit(l) -> "{" ^ (String.concat ", " (List.map string_of_sexpr l)) ^ "}"
      | SId(s) -> s
      | SArrayAccess(var, idx) -> var ^ "[" ^ (string_of_int idx) ^ "]"
      | SStringLit(s) -> "\"" ^ s ^ "\""
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(sassign) -> string_of_sassign sassign
      | SArrayAssign(v, i, e) -> v ^ "[" ^ (string_of_int i) ^ "]" ^ " = " ^ string_of_sexpr e
      | SCall(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
      | SFuncExpr(expr) -> string_of_sfdecl expr
      | SClosure(clsr) -> raise (Failure ("Closure is not implemented in sast"))) ^ ")"

      

and string_of_sassign (string, sexpr) = string ^ " "^ string_of_sexpr sexpr

and string_of_svdecl = function
    SDecl (typ, string) -> string_of_typ typ ^ " " ^ string
  | SInit (typ, sassign) -> string_of_typ typ ^ " " ^ string_of_sassign sassign
and string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SLocalVarDef l -> string_of_svdecl l ^ ";\n"

and string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map string_of_svdecl fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

and string_of_sdef = function
    SVarDef(svar_def) -> string_of_svdecl svar_def
  | SFuncDef(sfunc_def) -> string_of_sfdecl sfunc_def

let string_of_sprogram (defs) =
  let (globals, functions)  = defs
  in
  let result = globals@functions 
  in
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_sdef result) ^ "\n"

let extract_svar = function
    SDecl (typ, string) -> (typ, string)
  | SInit(typ, sassign) -> (typ, fst sassign)