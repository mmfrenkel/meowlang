(* Abstract Syntax Tree *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Greater | And | Or | Concat

type uop = Not

type typ = Int | Bool | Float | String | Void

type bind = typ * string

type import = Module of string

type expr =
    ILiteral of int
  | Fliteral of string
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list

type stmt =
    Expr of expr
  | Return of expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = import list * func_decl list
