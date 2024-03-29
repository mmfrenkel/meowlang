%{
open Ast
%}

%token RETURN MODULE IMPORT CALL FUNCTION DEF COMP CLASS NEW FREE MAKE HERE SIZE
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE ASSIGN LBRACKET RBRACKET
%token NOT EQ NEQ LT GT AND OR CONCAT CONTAINS IN
%token IF THEN ELSE FOR INCREMENT DECREMENT INT BOOL FLOAT STRING ARRAY
%token <int> ILIT
%token <bool> BLIT
%token <string> ID FLIT SLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc ARRAY
%right ASSIGN
%right CONCAT
%right OR AND
%right EQ NEQ
%right LT GT
%right PLUS MINUS
%right TIMES DIVIDE
%right NOT

%%

program:
  decls EOF                   { $1 }

decls:
    imports udf_udcs          { (List.rev $1, fst $2, snd $2) }
  | udf_udcs                  { ([], fst $1, snd $1)          }

imports:
    import                   { [$1] }
  | imports import           { $2 :: $1 }

import:
  MODULE ID IMPORT           { $2 }

udf_udcs:
   /* nothing */             { ([], [])                 }
| udf_udcs fdecl             { (($2 :: fst $1), snd $1) }
| udf_udcs cdecl             { (fst $1, ($2 :: snd $1)) }

fdecls:
    fdecl                    { [$1]     }
  | fdecls fdecl             { $2 :: $1 }

fdecl:
    LBRACE DEF return_type FUNCTION ID formals_opt RPAREN vdecls stmt_list RBRACE
      {
        {
          typ = $3;
          fname = $5;
          formals = List.rev $6;
          locals = List.rev $8;
          body = List.rev $9;
        }
      }

return_type:
    /* nothing */            { Void }
  | typ                      { $1   }

formals_opt:
    /* nothing */            { [] }
  | LPAREN formal_list       { $2 }

formal_list:
    typ ID                   { [($1,$2)]      }
  | formal_list COMMA typ ID { ($3,$4) :: $1  }

typ:
    INT                      { Int                                 }
  | BOOL                     { Bool                                }
  | FLOAT                    { Float                               }
  | STRING                   { String                              }
  | ID                       { Obtype ($1)                         }
  /* This is to support arrays as arguments to functions; -1 is just a 'dummy' value,
     since ocaml doesn't allow null for integer values */
  | ARRAY CONTAINS typ       { Arrtype (ILiteralArraySize(-1), $3) }

vdecls:
    /* nothing */             { []       }
  | vdecls vdecl              { $2 :: $1 }

vdecl:
    DEF typ ID SEMI                      { ($2, $3, Noexpr) }
  | DEF typ ID ASSIGN expr SEMI          { ($2, $3, $5)     }
  | DEF typ ID ASSIGN function_call SEMI { ($2, $3, $5)     }

stmt_list:
    /* nothing */             { []        }
  | stmt_list stmt            { $2 :: $1  }

stmt:
    expr SEMI                                     { Expr($1)           }
  | function_call SEMI                            { Expr($1)           }
  | RETURN expr SEMI                              { Return($2)         }
  | LBRACE stmt_list RBRACE                       { Block(List.rev $2) }
  | array_decl SEMI                               { Expr($1)           }
  | c_instance SEMI                               { Expr($1)           }
  | expr IF THEN LBRACE stmt_list RBRACE %prec NOELSE
      { If($1, Block(List.rev $5), Block([]))          }
  | expr IF THEN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE
      { If($1, Block(List.rev $5), Block(List.rev $9)) }
  | FOR expr INCREMENT expr_opt COMMA expr LBRACE stmt_list RBRACE
      { For(Increment, $2, $4, $6, Block(List.rev $8)) }
  | FOR expr DECREMENT expr_opt COMMA expr LBRACE stmt_list RBRACE
      { For(Decrement, $2, $4, $6, Block(List.rev $8)) }
  | ID IN ID ASSIGN expr SEMI                     { ClassAssign(Id($3), $1, $5)}
  | ID LBRACKET expr RBRACKET ASSIGN expr SEMI    { ArrayAssign($1, $3, $6)    }
  | FREE ID SEMI                                  { Dealloc(Id($2))            }
  | ID ASSIGN function_call SEMI                  { Expr(Assign(Id($1), $3))   }

expr:
    ILIT                      { ILiteral($1)                }
  | FLIT                      { Fliteral($1)                }
  | BLIT                      { BoolLit($1)                 }
  | SLIT                      { StringLit($1)               }
  | ID                        { Id($1)                      }
  | ID ASSIGN typ expr        { Assign(Id($1), Cast($3, $4))}
  | ID ASSIGN expr            { Assign(Id($1), $3)          }
  | PLUS expr COMMA expr      { Binop($2, Add,   $4)        }
  | MINUS expr COMMA expr     { Binop($2, Sub,   $4)        }
  | TIMES expr COMMA expr     { Binop($2, Mult,  $4)        }
  | DIVIDE expr COMMA expr    { Binop($2, Div,   $4)        }
  | EQ expr COMMA expr        { Binop($2, Equal, $4)        }
  | NEQ expr COMMA expr       { Binop($2, Neq,   $4)        }
  | LT expr COMP expr         { Binop($2, Less,  $4)        }
  | GT expr COMP expr         { Binop($2, Greater, $4)      }
  | AND expr COMMA expr       { Binop($2, And,   $4)        }
  | OR expr COMMA expr        { Binop($2, Or,    $4)        }
  | NOT expr                  { Unop(Not, $2)               }
  | LPAREN expr RPAREN        { $2                          }
  | CONCAT expr COMMA expr    { Binop($2, Concat, $4)       }
  | ID IN ID                  { ClassAccess(Id($3), $1)     }
  | ID LBRACKET expr RBRACKET { ArrayAccess($1, $3)         }

 function_call:
    CALL ID                          { FunctionCall($2, [])           }
  | CALL ID IN expr                  { MethodCall($4, $2, [])         }
  | CALL ID IN HERE                  { MethodCall(Id("this"), $2, []) }
  | CALL ID LPAREN args_opt          { FunctionCall($2, $4)           }
  | CALL ID IN ID LPAREN args_opt    { MethodCall(Id($4), $2, $6)     }
  | CALL ID IN HERE LPAREN args_opt  { MethodCall(Id("this"), $2, $6) }

expr_opt:
  /* nothing */               { Noexpr }
  | expr                      { $1     }

args_opt:
  | args_list                 { List.rev $1 }

args_list:
    expr                      { [$1]     }
  | args_list COMMA expr      { $3 :: $1 }

/* Array Specification */

array_decl:
    MAKE ID NEW ARRAY CONTAINS typ SIZE array_size_typ RPAREN LPAREN args_opt { NewArray($2, $6, $8, $11)}
  | MAKE ID NEW ARRAY CONTAINS typ SIZE array_size_typ                        { NewArray($2, $6, $8, []) }

array_size_typ:
    ILIT                      { ILiteralArraySize($1) }
  | ID                        { VariableArraySize($1) }

/* Classes */

cdecl:
    LBRACE DEF CLASS ID RPAREN vdecls methods RBRACE
    {
      {
        cname = $4;
        cvars = $6;
        cfuncs = $7;
      }
    }

methods:
    /* nothing */             { [] }
  | fdecls                    { $1 }

 /* Class Instantiation */

c_instance:
    MAKE ID NEW typ                  { NewInstance($2, $4, []) }
  | MAKE ID NEW typ RPAREN class_opt { NewInstance($2, $4, $6) }

class_opt:
    /* nothing */             { [] }
  | LPAREN copt_list          { $2 }

copt_list:
    expr                      { [$1]     }
  | copt_list COMMA expr      { $3 :: $1 }
