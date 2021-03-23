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
  | SAssign of string * sexpr
  | SFunctionCall of string * sexpr list
  | SMethodCall of string * string * sexpr list
  | SNewArray of string * typ * array_size * sexpr list
  | SNoexpr
  | SNewInstance of string * typ * sexpr list
  | SClassAccess of string * string
  | SArrayAccess of string * sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of op * sexpr * sexpr * sexpr * sstmt
  | SDealloc of string
  | SClassAssign of string * string * sexpr
  | SArrayAssign of string * sexpr

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind_formals list;
    slocals : bind_var list;
    sbody : sstmt list;
  }

type sclass_decl = {
    scname : string;
    scvars : bind_var list;
    scfuncs : func_decl list;
}

type program = import list * sfunc_decl list * sclass_decl list
