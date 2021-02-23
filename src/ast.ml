(* Abstract Syntax Tree *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Greater | And | Or | Concat | Increment | Decrement

type uop = Not

type typ = Int | Bool | Float | String | Void 
(*| Obtyp of string*)

type bind = typ * string

type import = Module of string

type array_size =
    ILiteralArraySize of int
  | VariableArraySize of string

type expr =
    ILiteral of int
  | Fliteral of string
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Bind of bind
  | Assign of string * expr
  | BindAssign of bind * expr
  | Call of string * expr list
  | NewArray of string * typ * array_size * expr list
  | NewInstance of string
  | ClassAccess of string * string

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of op * expr * expr * stmt
  | Dealloc of expr
  | ClassAssign of string * string * expr


type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type class_decl = {
    cname : string;
    cvars : bind list;
    cfuncs : func_decl list; 
}

type program = import list * func_decl list * class_decl list 
