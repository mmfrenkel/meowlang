%{
open Ast
%}

%token RETURN MODULE IMPORT CALL FUNCTION DEF COMP
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT EQ NEQ LT GT AND OR CONCAT
%token INT BOOL FLOAT STRING
%token <int> ILIT
%token <bool> BLIT
%token <string> ID FLIT SLIT
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left CONCAT
%left OR
%left AND
%left EQ NEQ
%left LT GT
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%%

program:
  decls EOF                     { $1                       }

/*
  A program is a list of import statements and a set of function declarations.
 */
decls:
  /* nothing */                 { ([], [])                 }
  | decls import                { (($2 :: fst $1), snd $1) }
  | decls fdecl                 { (fst $1, ($2 :: snd $1)) }

import:
  MODULE ID IMPORT              { Module($2)               }

/*
 A function declaration can include return types or not. It can include a list
 of parameters or not; or any combination of the two. Because we do not require
 users to use 'void', we have four possibilities for function declarations.
 */
fdecl:
    LBRACE DEF typ FUNCTION ID formals_opt RPAREN vdecl_list stmt_list RBRACE
      {
        {
          typ = $3;
          fname = $5;
          formals = List.rev $6;
          locals = List.rev $8;
          body = List.rev $9;
        }
      }
  | LBRACE DEF FUNCTION ID formals_opt RPAREN vdecl_list stmt_list RBRACE
      {
        {
          typ = Void;
	        fname = $4;
	        formals = List.rev $5;
	        locals = List.rev $7;
	        body = List.rev $8;
        }
      }
  | LBRACE DEF typ FUNCTION ID RPAREN vdecl_list stmt_list RBRACE
      {
        {
          typ = $3;
          fname = $5;
          formals = [];
          locals = List.rev $7;
          body = List.rev $8;
        }
      }
  | LBRACE DEF FUNCTION ID RPAREN vdecl_list stmt_list RBRACE
      {
        {
          typ = Void;
	        fname = $4;
	        formals = [];
	        locals = List.rev $6;
	        body = List.rev $7;
        }
      }

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

vdecl_list:
    /* nothing */             { []                     }
  | vdecl_list vdecl          { $2 :: $1               }

vdecl:
   DEF typ ID SEMI            { ($2, $3)               }

stmt_list:
    /* nothing */             { []                     }
  | stmt_list stmt            { $2 :: $1               }

stmt:
    expr SEMI                 { Expr($1)               }
  | RETURN expr SEMI          { Return($2)             }

expr_opt:
  | expr                      { $1                     }

expr:
    ILIT                      { ILiteral($1)           }
  | FLIT	                    { Fliteral($1)           }
  | BLIT                      { BoolLit($1)            }
  | SLIT                      { StringLit($1)          }
  | ID                        { Id($1)                 }
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
  | ID ASSIGN expr            { Assign($1, $3)         }
  | CALL ID                   { Call($2, [])           }
  | CALL ID LPAREN args_opt   { Call($2, $4)           }
  | LPAREN expr RPAREN        { $2                     }
  | expr CONCAT expr          { Binop($1, Concat, $3)  }

args_opt:
    /* nothing */             { []                     }
  | args_list                 { List.rev $1            }

args_list:
    expr                      { [$1]                   }
  | args_list COMMA expr      { $3 :: $1               }
