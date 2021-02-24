%{
open Ast
%}

%token RETURN MODULE IMPORT CALL FUNCTION DEF COMP CLASS NEW FREE MAKE
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE ASSIGN
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
    imports fdecls cdecls EOF { (List.rev $1, List.rev $2, List.rev $3) }
  | fdecls cdecls EOF         { ([], List.rev $1, List.rev $2) }
  | imports fdecls EOF        { (List.rev $1, List.rev $2, []) }
  | fdecls EOF                { ([], List.rev $1, []) }
  | cdecls EOF                { ([], [], List.rev $1) }

imports:
    import                   { [$1]                    }
  | imports import           { $2 :: $1                }

import:
  MODULE ID IMPORT           { Module($2)              }

fdecls:
    fdecl                    { [$1]                    }
  | fdecls fdecl             { $2 :: $1                }

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
    /* nothing */            { Void                   }
  | typ                      { $1                     }

formals_opt:
    /* nothing */            { []                     }
  | LPAREN formal_list       { $2                     }

formal_list:
    typ ID                   { [($1,$2)]               }
  | formal_list COMMA typ ID { ($3,$4) :: $1           }

typ:
    INT     { Int    }
  | BOOL    { Bool   }
  | FLOAT   { Float  }
  | STRING  { String }
  | ID      { Obtype ($1) }

vdecls:
    /* nothing */             { []                     }
  | vdecls vdecl              { $2 :: $1               }

vdecl:
    DEF typ ID SEMI           { ($2, $3, Noexpr)       }
  | DEF typ ID ASSIGN expr SEMI { ($2, $3, $5)         }

stmt_list:
    /* nothing */             { []                     }
  | stmt_list stmt            { $2 :: $1               }

stmt:
    expr SEMI                 { Expr($1)               }
  | RETURN expr SEMI          { Return($2)             }
  | LBRACE stmt_list RBRACE   { Block(List.rev $2)     }
  | CALL ID SEMI              { Expr(Call($2, []))     }
  | CALL ID LPAREN args_opt SEMI { Expr(Call($2, $4))  }
  | array_decl SEMI           { Expr($1)               }
  | c_instance SEMI           { Expr($1)               }
  | expr IF THEN stmt %prec NOELSE { If($1, $4, Block([]))  }
  | expr IF THEN stmt ELSE stmt { If($1, $4, $6)            }
  | FOR expr INCREMENT expr_opt COMMA expr stmt { For(Increment, $2, $4, $6, $7) }
  | FOR expr DECREMENT expr_opt COMMA expr stmt { For(Decrement, $2, $4, $6, $7) }
  | ID IN ID ASSIGN expr SEMI { ClassAssign($1, $3, $5)}
  | FREE expr                 { Dealloc($2)            }

expr:
    ILIT                      { ILiteral($1)           }
  | FLIT	                    { Fliteral($1)           }
  | BLIT                      { BoolLit($1)            }
  | SLIT                      { StringLit($1)          }
  | ID                        { Id($1)                 }
  | ID ASSIGN expr            { Assign($1, $3)         }
  | PLUS expr COMMA expr      { Binop($2, Add,   $4)   }
  | MINUS expr COMMA expr     { Binop($2, Sub,   $4)   }
  | TIMES expr COMMA expr     { Binop($2, Mult,  $4)   }
  | DIVIDE expr COMMA expr    { Binop($2, Div,   $4)   }
  | EQ expr COMMA expr        { Binop($2, Equal, $4)   }
  | NEQ expr COMMA expr       { Binop($2, Neq,   $4)   }
  | LT expr COMP expr         { Binop($2, Less,  $4)   }
  | GT expr COMP expr         { Binop($2, Greater, $4) }
  | AND expr COMMA expr       { Binop($2, And,   $4)   }
  | OR expr COMMA expr        { Binop($2, Or,    $4)   }
  | NOT expr                  { Unop(Not, $2)          }
  | LPAREN expr RPAREN        { $2                     }
  | CONCAT expr COMMA expr    { Binop($2, Concat, $4)  }
  | ID IN ID                  { ClassAccess($1, $3)    }

expr_opt:
  /* nothing */               { Noexpr                 }
  | expr                      { $1                     }

args_opt:
  | args_list                 { List.rev $1            }

args_list:
    expr                      { [$1]                   }
  | args_list COMMA expr      { $3 :: $1               }

/* Array Specification */

array_decl:
    MAKE ID NEW typ ARRAY CONTAINS array_size_typ RPAREN LPAREN args_opt { NewArray($2, $4, $7, $10) }
  | MAKE ID NEW typ ARRAY CONTAINS array_size_typ  { NewArray($2, $4, $7, []) }
  | MAKE ID NEW typ ARRAY { NewArray($2, $4, ILiteralArraySize(0), []) }

array_size_typ:
    ILIT                      { ILiteralArraySize($1)   }
  | ID                        { VariableArraySize($1)   }

/* Classes */

cdecls:
    cdecl                     { [$1]                    }
  | cdecls cdecl              { $2 :: $1                }

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
    /* nothing */             { []                      }
  | fdecls                    { $1                      }

 /* Class Instantiation */

c_instance:
    MAKE ID NEW ID                           { NewInstance($4) }
  | MAKE ID NEW ID RPAREN LPAREN class_opt   { NewInstance($4) }

class_opt:
    /* nothing */             { []                      }
  | LPAREN copt_list          { $2                      }

copt_list:
    expr                      { [$1]                    }
  | copt_list COMMA expr      { $3 :: $1                }
