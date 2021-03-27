(* Semantically Checked AST *)
open Ast

type sexpr = typ * sx
and sx =
    SILiteral of int
  | SFliteral of string
  | SBoolLit of bool
  | SStringLit of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of sexpr * sexpr
  | SFunctionCall of string * sexpr list
  | SMethodCall of string * string * sexpr list
  | SNewArray of string * typ * array_size * sexpr list
  | SNoexpr
  | SNewInstance of string * typ * sexpr list
  | SClassAccess of typ * sexpr * string
  | SArrayAccess of string * sexpr

  type sbind_var = typ * string * sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of op * sexpr * sexpr * sexpr * sstmt
  | SDealloc of sexpr
  | SClassAssign of typ * sexpr * string * sexpr
  | SArrayAssign of string * sexpr * sexpr

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind_formals list;
    slocals : bind_var list; (* this is still a bind_var because exprs get moved to body *)
    sbody : sstmt list;
  }

type sclass_decl = {
    scname : string;
    scvars : sbind_var list;
    scfuncs : sfunc_decl list;
}

type program = import list * sfunc_decl list * sclass_decl list
