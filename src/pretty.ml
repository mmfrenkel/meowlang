(*
   Pretty-printing functions that allow you to print a meowlang program in the
   near-equivalent C program. Note that a large amount of this code was derived
   and adjusted from the MicroC Compiler provided by S. Edwards (Spring 2021).
*)
open Ast

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Greater -> ">"
  | And -> "&&"
  | Or -> "||"
  | Concat -> "+"
  | Increment -> "++"
  | Decrement -> "--"
let string_of_modules = function
  Module(l) -> "include \"" ^ l ^ "\""

let string_of_uop = function
  Not -> "!"
let string_of_typ = function
  Int -> "int"
| Bool -> "bool"
| Float -> "float"
| String -> "char *"
| Void -> ""
| Obtyp(s) -> "Class " ^ s 

let string_of_array_size = function
  ILiteralArraySize(l) -> string_of_int l
| VariableArraySize(s) -> s

let rec string_of_expr = function
    ILiteral(l) -> string_of_int l
  | StringLit(l) -> "\"" ^ l ^ "\""
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Bind(t, s) -> string_of_typ t ^ " " ^ s 
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | BindAssign((t, s), e) -> string_of_typ t ^ s ^ " = " ^ string_of_expr e 
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | NewArray(i, typ, s, contents) ->
      string_of_typ typ ^ " [" ^ string_of_array_size s ^ "] " ^ i ^ " = [ " ^ String.concat ", " (List.map string_of_expr contents) ^ " ]"
  | NewInstance(s) -> "Class " ^ s 
  | ClassAccess(ob, el) -> ob ^ "." ^ el

let rec string_of_stmt = function
    Expr(expr) -> "\t" ^ string_of_expr expr ^ ";\n"
  | Return(expr) -> "\treturn " ^ string_of_expr expr ^ ";\n"
  | Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | If(e, s, Block([])) -> "\tif (" ^ string_of_expr e ^ ") {\n\t" ^ string_of_stmt s ^ "\t}\n"
  | If(e, s1, s2) ->  "\tif (" ^ string_of_expr e ^ ") {\n\t" ^
    string_of_stmt s1 ^ "\t}\n\telse {\n\t" ^ string_of_stmt s2 ^ "\t}\n"
  | For(o, e1, e2, s) ->
      "\tfor (" ^ string_of_expr e1 ^ string_of_op o ^ "; " ^ string_of_expr e2 ^ ") {\n\t\t" ^ string_of_stmt s ^ "\t}\n"
  | Dealloc(e) -> "free(" ^ string_of_expr e ^ ")"
  | ClassAssign(s1, s2, e) -> s1 ^ "." ^ s2 ^ " = " ^ string_of_expr e 

let string_of_vdecl (t, id) = "\t" ^ string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_formals (t, id) = string_of_typ t ^ " " ^ id

let format_params fdecl =
  "(" ^ String.concat ", " (List.map string_of_formals fdecl.formals) ^ ")\n"

let string_of_fdecl fdecl =
  let return_string = (
    match fdecl.typ with
        Void -> ""
      | _ -> string_of_typ fdecl.typ ^ " ") in

  return_string ^ fdecl.fname ^ format_params fdecl ^
  "{\n" ^
    String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"


let string_of_cdecl cdecl = 
  "Class " ^ cdecl.cname ^ "{\n" ^ 
  String.concat "" (List.map string_of_stmt cdecl.cvars) ^
  String.concat "" (List.map string_of_fdecl cdecl.cfuncs) ^ 
  "}\n"



let string_of_program (imports, funcs, classes) =
  String.concat "" (List.map string_of_modules imports) ^ "\n\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
String.concat "\n" (List.map string_of_cdecl classes) 