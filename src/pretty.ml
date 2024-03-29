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
  l -> "include \"" ^ l ^ "\""

let string_of_uop = function
  Not -> "!"

let string_of_array_size = function
  ILiteralArraySize i -> string_of_int i
| VariableArraySize s -> s

let rec string_of_typ = function
  Int -> "int"
| Bool -> "bool"
| Float -> "float"
| String -> "char *"
| Void -> ""
| Obtype(s) -> "class " ^s
| Arrtype(size, typ) ->
    match size with
      ILiteralArraySize i when i = -1 -> string_of_typ typ ^ " []"
    | _ -> string_of_typ typ ^ " [" ^ string_of_array_size size ^ "]"

let string_of_array_size = function
  ILiteralArraySize(l) -> string_of_int l
| VariableArraySize(s) -> s

let rec string_of_expr = function
    ILiteral(l) -> string_of_int l
  | StringLit(l) -> "\"" ^ l ^ "\""
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Cast(typ, e) -> "(" ^ string_of_typ typ ^ ") " ^ string_of_expr e
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | FunctionCall(f, el) ->
      (match f with
          "Meow" -> "printf" ^ "(\"%X\\n\", " ^ String.concat ", " (List.map string_of_expr el) ^ ")"
        | _      -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")")
  | MethodCall(ob, f, el) ->
    string_of_expr ob ^ "." ^ f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | NewArray(i, typ, s, contents) ->
      string_of_typ typ ^ " [" ^ string_of_array_size s ^ "] " ^ i ^ " = [ " ^
      String.concat ", " (List.map string_of_expr contents) ^ " ]"
  | Noexpr -> ""
  | NewInstance(var, c, []) -> string_of_typ c ^ " " ^ var
  | NewInstance(var, c, exprs) -> string_of_typ c ^ " " ^ var ^ "(" ^
      String.concat "" (List.map (fun e -> string_of_expr e ^ ", ") exprs) ^ ")"
  | ClassAccess(ob, el) -> string_of_expr ob ^ "." ^ el
  | ArrayAccess(var, e) -> var ^ "[" ^ string_of_expr e ^ "]"

let rec string_of_stmt = function
    Expr(expr) -> "\t" ^ string_of_expr expr ^ ";\n"
  | Return(expr) -> "\treturn " ^ string_of_expr expr ^ ";\n"
  | Block(stmts) ->
    "\t{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "\t}\n"
  | If(e, s, Block([])) -> "\tif (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "\tif (" ^ string_of_expr e ^ ")" ^
    string_of_stmt s1 ^ "\telse\t" ^ string_of_stmt s2
  | For(o, e1, e_opt, e2, s) ->
      "\tfor (" ^string_of_expr e_opt ^ " " ^ string_of_expr e1 ^ string_of_op o ^
      " " ^ string_of_expr e2 ^ ") {\n\t\t" ^ string_of_stmt s ^ "\t}\n"
  | Dealloc(e) -> "\tfree(" ^ string_of_expr e ^ ");\n"
  | ClassAssign(e1, s2, e2) -> "\t" ^ string_of_expr e1 ^ "." ^ s2 ^ " = " ^ string_of_expr e2 ^ ";\n"
  | ArrayAssign(s, e1, e2) -> "\t" ^ s ^ "[" ^ string_of_expr e1 ^ "] = " ^ string_of_expr e2 ^";\n"

let string_of_vdecl (t, id, expr) =
    match expr with
    | Noexpr -> "\t" ^ string_of_typ t ^ " " ^ id ^ ";\n"
    | _ -> "\t" ^ string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr expr ^ ";\n"

let string_of_formals (t, id) = string_of_typ t ^ " " ^ id

let format_params fdecl =
  "(" ^ String.concat ", " (List.map string_of_formals fdecl.formals) ^ ")\n"

let string_of_fdecl fdecl  =
  let return_string = (
    match fdecl.typ with
      Void -> ""
    | _ -> string_of_typ fdecl.typ ^ " ") in

  return_string ^ fdecl.fname ^ format_params fdecl ^
  "{\n" ^
    String.concat "" (List.map string_of_vdecl fdecl.locals) ^
    String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_mdecl fdecl =
  let indent = "\t" in

  let return_string = (
    match fdecl.typ with
        Void -> ""
      | _ -> string_of_typ fdecl.typ ^ " ") in

  indent ^ return_string ^ fdecl.fname ^ format_params fdecl ^ indent ^ "{\n" ^
  String.concat indent (List.map string_of_vdecl fdecl.locals) ^
  String.concat indent (List.map string_of_stmt fdecl.body) ^ indent ^ "}\n"

let string_of_cdecl cdecl =
    "Class " ^ cdecl.cname ^ " {\n\n" ^
    String.concat "" (List.map string_of_vdecl cdecl.cvars) ^ "\n" ^
    String.concat "" (List.map string_of_mdecl cdecl.cfuncs) ^ "\n" ^
    "}\n"

(* Main function that pretty-prints an AST *)
let string_of_program (imports, funcs, classes) =
  String.concat "" (List.map string_of_modules imports) ^ "\n\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_cdecl classes)
