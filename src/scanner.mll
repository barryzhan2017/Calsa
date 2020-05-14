(* Ocamllex scanner for MicroC *)

{ open Microcparse }

let digit = ['0'-'9']
let digits = digit+
let float = digits '.' digits

let lower_case = ['a'-'z'] 
let upper_case = ['A'-'Z']
let letter = lower_case | upper_case
let letters = letter+

let whitespace = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"

let id = letter (digit | letter | '_')*

rule token = parse
  whitespace | newline { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
(* COMMA *)
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { MULTIPLY }
| '/'      { DIVIDE }
| '%'      { MODULO }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "&&"     { AND }
| "||"     { OR }
| "."      { DOT }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
(* RETURN *)
| "return" { RETURN }
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "void"   { VOID }
| "string" { STRING }
| "List"   { LIST }
| "Set"    { SET } 
| "true"   { BOOLLIT(true) }
| "false"  { BOOLLIT(false) }
| float as lem { FLOATLIT(float_of_string lem) }
| digits as lem  { INTLIT(int_of_string lem) }
| id as lem { ID(lem) }
| '\"' { STRINGLIT ("\"" ^ (string (Buffer.create 128) lexbuf) ^ "\"") }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and string buffer = parse
  '\"' { Buffer.contents buffer }
| newline | whitespace | [^ '"']+ as lem { Buffer.add_string buffer lem; string buffer lexbuf }