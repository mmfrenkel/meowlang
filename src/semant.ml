(*
  Performs semantic checks on the AST, producing a new SAST
*)
open Exceptions
open Ast
open Sast
open Pretty

module StringMap = Map.Make(String)

(* Hashtable of valid functions and classes to be used globally *)
let function_tbl:(string, Ast.func_decl) Hashtbl.t = Hashtbl.create 10
let class_tbl:(string, Ast.class_decl) Hashtbl.t = Hashtbl.create 10

(* Return function by name; Raise exception if it doesn't exist *)
let find_function fname =
  try Hashtbl.find function_tbl fname
  with Not_found -> raise (FunctionNotFound (undeclared_msg ^ fname))

(* Return class by name; Raise exception if it doesn't exist *)
let find_class cname =
  try Hashtbl.find class_tbl cname
  with Not_found -> raise (ClassNotFound (undeclared_msg ^ cname))

(* Return variable by name; Raise exception if it doesn't exist *)
  let find_variable tbl vname =
    try Hashtbl.find tbl vname
    with Not_found -> raise (VariableNotFound (undeclared_msg ^ vname))

(* Raise an exception if the given types are not the same *)
let check_matching_types typ1 typ2 err =
  if typ1 = typ2 then typ1 else raise err

(* Helper function to check for duplicates of anything *)
let find_duplicate items exception_msg =
  let rec helper = function
      n1 :: n2 :: _ when n1 = n2 -> raise (DuplicateIdentifier (exception_msg ^ n1 ^ "n"))
    | _ :: t -> helper t
    | [] -> ()
  in helper (List.sort compare items)

(* Find the type of something, given a symbol table *)
let find_type_of_id symbol_tbl id =
  try Hashtbl.find symbol_tbl id
  with Not_found -> raise (VariableNotFound (undeclared_msg ^ id))

(*Find the class method by method name and class type *)
let find_class_method cname mname =
  let cls = find_class cname in
  let cls_methods = List.fold_left (fun m cls_method -> StringMap.add cls_method.fname cls_method m) StringMap.empty (cls.cfuncs) in
  try StringMap.find mname cls_methods
  with Not_found -> raise (ClassMethodNotFound(class_method_unknown))

let rec semant_expr expr symbol_tbl =

  (* Checks that argument types match formal var types *)
  let check_arg_type formal_param arg_expr =
    let (actual_type, arg_expr') = semant_expr arg_expr symbol_tbl
    and expected_type = fst formal_param
    in
    if actual_type = expected_type
      then (actual_type, arg_expr')
    else raise (FunctionArgumentTypeMismatch("" ^ string_of_expr arg_expr))
  in

  match expr with
    ILiteral i -> (Int, SILiteral i)
  | Fliteral f -> (Float, SFliteral f)
  | BoolLit b -> (Bool, SBoolLit b)
  | StringLit s -> (String, SStringLit s)
  | Id id -> (find_type_of_id symbol_tbl id, SId id)
  | Noexpr -> (Void, SNoexpr)

  | Binop (e1, op, e2) as ex ->
      (* Binary operations work with operands of the same type *)
      let (typ1, e1') = semant_expr e1 symbol_tbl
      and (typ2, e2') = semant_expr e2 symbol_tbl in
      let same_type = typ1 = typ2 in
      let end_typ = match op with
          Add | Sub | Mult | Div | Increment | Decrement when same_type && typ1 = Int -> Int
        | Add | Sub | Mult | Div when (typ1 = Float || typ1 = Int) && (typ2 = Float || typ2 = Int) -> Float (* This is casting from float to int *)
        | Equal | Neq     when same_type -> Bool
        | Less | Greater  when same_type && (typ1 = Float || typ1 = Int) && (typ2 = Float || typ2 = Int) -> Bool
        | And | Or        when same_type && typ1 = Bool -> Bool
        | Concat          when (typ1 = String && (typ2 = String || typ2 == Int || typ2 == Float)) ||
                               (typ2 = String && (typ1 = String || typ1 == Int || typ1 == Float)) -> String
        | _               -> raise (IllegalBinaryOp(string_of_expr ex))
    in (end_typ, SBinop((typ1, e1'), op, (typ2, e2')))

  | Unop (op, e) as ex ->
    (* Only one type of Uop supported *)
      let (vtype, e') = semant_expr e symbol_tbl in
      if vtype == Bool then (vtype, SUnop(op, (vtype, e')))
      else raise (IllegalUnaryOp (string_of_expr ex))

  | Assign (var, e) as ex ->
    (* Check that the expr produces the same type as the variable it is assigned to *)
      let var_typ = find_type_of_id symbol_tbl var
      and (ret_type, e') = semant_expr e symbol_tbl in
      let err = VariableAssignmentError(string_of_expr ex)
      in (check_matching_types var_typ ret_type err, SAssign(var, (ret_type, e')))

  | FunctionCall (fname, args) ->
      (* 1. Make sure function exists *)
      let func = find_function fname in

      (* 2. Check that param length is equal to the num args provided *)
      if List.length args != List.length func.formals
        then raise (FunctionArgumentLengthMismatch (func_arg_num_mismatch ^ fname))
      else
      (* 3. Check that the arguments passed are of the expected type *)
        let args' = List.map2 check_arg_type func.formals args
        in (func.typ, SFunctionCall(fname, args'))

  | MethodCall (vname, mname, args) as ex ->
      (* 1. Check that the object exists in the symbol table  *)
      let v_type = find_type_of_id symbol_tbl vname in

      (* 2. Check that the method exists within the class and get it *)
      let meth =
        match v_type with
        | Obtype object_type -> find_class_method object_type mname
        | _ -> raise (InvalidMethodCall(invalid_method_call ^ string_of_expr ex))
      in
      (* 3. Check that param length is equal to the num args provided *)
      if List.length args != List.length meth.formals
        then raise (MethodArgumentLengthMismatch(meth_arg_num_mismatch ^ mname))
      else
      (* 4. Check that the arguments passed are of the expected type *)
        let args' = List.map2 check_arg_type meth.formals args in
        (meth.typ, SMethodCall(vname, mname, args'))

  | NewArray (arr_name, arr_typ, arr_size, expr_list) as ex ->

      (* 1. Check to make sure that size is integer *)
      let array_size_typ =
        match arr_size with
          ILiteralArraySize i ->
            (* 2. check integer literal against expr_list length *)
            let num_items = List.length expr_list in
              if num_items > i
                then raise (ExcessArrayInput(excess_array_item ^ string_of_int num_items))
              else ();
            Int
        | VariableArraySize s -> find_type_of_id symbol_tbl s
      in
      if array_size_typ != Int
        then raise (InvalidArraySizeSpecified(invalid_array_size_msg ^ string_of_expr ex ))
      else
        let expr_list' = List.fold_left (fun acc e -> semant_expr e symbol_tbl :: acc) [] expr_list
        in
        (* 3. if expr_list, check that the expressions match the content type of array *)
        List.iter (
          fun (expr_typ, expr) ->
            if expr_typ != arr_typ
              then raise (InvalidArrayItem(invalid_array_item_msg ^ string_of_expr ex))
            else ()
        ) expr_list';

        (* make sure to add the allocated obj to symbol table so it can be referenced *)
        Hashtbl.add symbol_tbl arr_name (Arrtype (arr_size, arr_typ));
        (Arrtype(arr_size, arr_typ), SNewArray(arr_name, arr_typ, arr_size, expr_list'))

  | NewInstance (obj_name, typ, expr_list) as ex ->

      (* 1. You can only create an "instance" of something that is type Objtype *)
      let cname = match typ with
          Obtype o -> o
        | _ -> raise (ObjectCreationInvalid(invalid_object_creation ^ string_of_expr ex))
      and expr_list' = List.fold_left (fun acc e -> semant_expr e symbol_tbl :: acc) [] expr_list
      in
      (* 2. Check that the class of new instance actually exists - valid*)
      let _ = find_class cname in

      (* make sure to add the allocated obj to symbol table so it can be referenced *)
      Hashtbl.add symbol_tbl obj_name (Obtype (cname));
      (typ, SNewInstance(obj_name, typ, expr_list'))

  | ClassAccess (obj_name, class_var) as ex ->
      (* 1. Check that the object exists in the symbol table  *)
      let typ = find_type_of_id symbol_tbl obj_name in

      (* 2. You can only "access" instance variables of type Obtype *)
      let cname = match typ with
          Obtype o -> o
        | _ -> raise (InstanceVariableAccessInvalid(invalid_instance_var_access ^ string_of_expr ex))
      in
      (* 3. Check that the instance variable exists within the class *)
      (Obtype(cname), (SClassAccess(obj_name, class_var)))

(*
let rec semant_stmt stmt symbol_tbl =

  (* THIS NEEDS TO BE FILLED OUT!!!! See microC for details removed *)
  match stmt with
    Expr e -> SExpr (semant_expr e symbol_tbl)
  | Return e -> SReturn()
  | If (e, stmt1, stmt2) -> SIf()
  | For (op, e1, e2, e3, stmt) -> SFor()
  | Dealloc id -> SDealloc(id)
  | ClassAssign (id, meth_name, e) -> SClassAssign()
  | Block b -> SBlock(semant_stmt b symbol_tbl)
*)

let check_function_body func =

  (* Build local symbol table of variables for this scope *)
  let symbol_table:(string, Ast.typ) Hashtbl.t = Hashtbl.create 10
  in
  List.iter (fun (typ, name) -> Hashtbl.add symbol_table name typ) func.formals;
  List.iter (fun (typ, name, expr) -> Hashtbl.add symbol_table name typ) func.locals

  (* TO DO: Build up the SAST Tree for the Function Here -- NEED STATEMENTS FILLED OUT *)
  (* semant_stmt func.body symbol_table *)

let check_function func =

  let list_formal_names = List.fold_left (fun acc form -> snd form :: acc) [] func.formals
  and list_locals_names = List.fold_left (fun acc (typ, name, expr) -> name  :: acc) [] func.locals
  in
  (* Check for duplicate formal and duplicate local variable names on their own *)
  find_duplicate (list_formal_names) dup_formal_msg;
  find_duplicate (list_locals_names) dup_local_var_msg;
  (* Check for duplicates together *)
  find_duplicate (list_formal_names @ list_locals_names) dup_form_local_msg;

  (* Check contents of function body *)
  check_function_body func

(* Checks for duplicates in class, functions and method names  *)
let check_duplicates functions classes =
  find_duplicate (List.map (fun f -> f.fname) functions) dup_func_msg;
  find_duplicate (List.map (fun c -> c.cname) classes) dup_class_msg;
  List.iter (fun cls -> find_duplicate (List.map (fun m -> m.fname) cls.cfuncs) dup_method_msg) classes;
  ()

let check (imports, functions, classes) =

  (* Check for duplicate function, method and class names *)
  check_duplicates functions classes;

  (* Create a maps of functions, classes *)
  List.iter (fun func -> Hashtbl.add function_tbl func.fname func) functions;
  List.iter (fun cls-> Hashtbl.add class_tbl cls.cname cls) classes;

  if Hashtbl.mem function_tbl "Main" then true else raise (MissingMainFunction (missing_main_func_msg))
