(* Abstract Syntax Tree *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Greater | And | Or | Concat | Increment | Decrement

type uop = Not

type array_size =
    ILiteralArraySize of int
  | VariableArraySize of string

type typ = Int | Bool | Float | String | Void | Obtype of string | Arrtype of array_size * typ

type import = string

type expr =
    ILiteral of int
  | Fliteral of string
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Cast of typ * expr
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | FunctionCall of string * expr list
  | MethodCall of expr * string * expr list
  | NewArray of string * typ * array_size * expr list
  | Noexpr
  | NewInstance of string * typ * expr list
  | ClassAccess of expr * string
  | ArrayAccess of string * expr

type bind_var = typ * string * expr
type bind_formals = typ * string

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of op * expr * expr * expr * stmt
  | Dealloc of expr
  | ClassAssign of expr * string * expr
  | ArrayAssign of string * expr * expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind_formals list;
    locals : bind_var list;
    body : stmt list;
  }

type class_decl = {
    cname : string;
    cvars : bind_var list;
    cfuncs : func_decl list;
}

type program = import list * func_decl list * class_decl list
