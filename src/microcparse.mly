/* Ocamlyacc parser for MicroC */

%{
open Ast
%}


%token SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE PLUS MINUS MULTIPLY DIVIDE MODULO ASSIGN QUOTATION DOT

%token EQ NEQ LT AND OR
%token IF ELSE WHILE INT VOID FLOAT BOOL STRING LIST
/* return, COMMA token */
%token RETURN COMMA
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BOOLLIT
%token <string> ID STRINGLIT
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULO
%left LPAREN RPAREN
%left DOT

%%

/* add function declarations*/
program:
  decls EOF { $1 }

decls:
   /* nothing */ { [] }
  | decl SEMI decls { $1 :: $3 }
  | decl decls { $1 :: $2 }

decl:
    vdecl     { VarDef($1) }
  | fdecl     { FuncDef($1)}

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
    typ ID      { Decl($1, $2) }
  | typ assign  { Init($1, $2) }

typ:
    INT   { Int   }
  | VOID  { Void  }
  | FLOAT { Float }
  | BOOL  { Bool  }
  | STRING {String}
  | typ LBRACKET INTLIT RBRACKET  { Array ($1, $3) }
  | LIST { List }

/* fdecl */
fdecl:
  typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp= $1;
      fname= $2;
      formals=$4;
      locals=$7;
      body=$8
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /* return */
  | RETURN expr SEMI                        { Return $2      }

expr:
    INTLIT          { Literal($1)            }
  | FLOATLIT        { FloatLit($1)           }
  | BOOLLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | ID LBRACKET INTLIT RBRACKET { ArrayAccess($1, $3) }
  | STRINGLIT     { StringLit(String.sub $1 1 ((String.length $1) - 2)) }
  | LBRACKET args_opt RBRACKET { ArrayLit($2) }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr MULTIPLY expr { Binop($1, Mul, $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr MODULO expr { Binop($1, Mod,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | assign   { Assign($1) }
  | ID LBRACKET INTLIT RBRACKET ASSIGN expr   { ArrayAssign($1, $3, $6) }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }
  /* Function calls like a.sum(), this works by interpreting a.sum(b, c) as sum(a, b, c) */
  | expr DOT ID LPAREN args_opt RPAREN { Call($3, $1::$5) }

assign:
  ID ASSIGN expr { ($1, $3) }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
