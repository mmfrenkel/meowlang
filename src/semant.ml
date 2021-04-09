(******************************************************************************)
(* Performs semantic checks on the AST, producing a new SAST                  *)
(* Also responsible for converting classes into structs and functions that    *)
(* are more easily processed in the LLVM step (i.e., call site adjustments    *)
(* and lifing methods to global space with new struct arguments)              *)
(******************************************************************************)
open Exceptions
open Ast
open Sast
open Pretty

module StringMap = Map.Make(String)

type environment = {
  in_class : bool;
  class_name: string;
  constructor: bool;
  obj_name: string; (* empty string if constructor if false *)
  mutable function_name: string;
  mutable returns: typ;
  symbols : (string, Ast.typ) Hashtbl.t;
}

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

(* Takes a class name to produce the variable name given to objects
   when passed as parameters to method flipped into functions. *)
let mangled_obj_varname cname =
  String.lowercase_ascii (cname ^ "*")

(* Converts a method name to a function name *)
let m_to_f_name cls_n m_n =
  cls_n ^ "." ^ m_n

(* Raise an exception if the given types are not the same *)
let check_matching_types typ1 typ2 err =
  (* with arrays we don't care if the sizes match; they're just implemented as pointers *)
  match typ1 with
    Arrtype(_, arr_typ_1) ->
      (match typ2 with
        Arrtype(_, arr_typ_2) when arr_typ_1 = arr_typ_2 -> typ1
      | _ -> raise err)
  | _ when typ1 = typ2 -> typ1
  | _ -> raise err

(* Helper function to check for duplicates of anything *)
let find_duplicate items exception_msg =
  let rec helper = function
      n1 :: n2 :: _ when n1 = n2 ->
        let msg = Printf.sprintf "%s %s" exception_msg n1
        in raise (DuplicateIdentifier (msg))
    | _ :: t -> helper t
    | [] -> ()
  in helper (List.sort compare items)

(* Find the type of something, given a symbol table *)
let find_type_of_id symbol_tbl id =
  try Hashtbl.find symbol_tbl id
  with Not_found ->
    let msg = Printf.sprintf "%s %s" undeclared_msg id
    in raise (VariableNotFound (msg))

(*Find the class method by method name and class type *)
let find_class_method cname mname =
  let cls = find_class cname in
  let cls_methods = List.fold_left (
      fun m cls_method -> StringMap.add cls_method.fname cls_method m
    ) StringMap.empty (cls.cfuncs)
  in
  try StringMap.find mname cls_methods
  with Not_found ->
    let msg = Printf.sprintf "%s %s" class_method_unknown (m_to_f_name cname mname)
    in raise (ClassMethodNotFound(msg))

let instance_variables_of_cls cls_name =
  let cls = find_class cls_name in
  List.fold_left (fun acc (_, name, _) -> name  :: acc) [] cls.cvars

(*** Checks for duplicates ****)
let check_duplicates functions classes =
  (* duplicates in function names *)
  find_duplicate (List.map (fun f -> f.fname) functions) dup_func_msg;
  (* duplicates in class names*)
  find_duplicate (List.map (fun c -> c.cname) classes) dup_class_msg;
  (* duplicates in methods within a class*)
  List.iter (fun cls -> find_duplicate (List.map (fun m -> m.fname) cls.cfuncs) dup_method_msg) classes;
  ()

(* Add built in functions to the list of functions *)
let add_built_ins existing_funcs =
  (*  printf function, corresponding to Meow in Meowlang *)
  let printf = {
    typ = Void;
    fname = "Meow";
    formals = [(String, "x")];
    locals = [];
    body = []
  }
  and custom_scanf = {
    typ = Int;
    fname = "Scan";
    formals = [(String, "x")];
    locals = [];
    body = []
  }
  in
  let e_funs = printf :: existing_funcs in
  custom_scanf :: e_funs

(* return boolean value to represent if return type is expected for a function *)
let has_return_value func =
  match func.typ with
    Void -> false
  | _ -> true


(****************************************)
(* Main function for checking semantics *)
(* of expressions                       *)
(****************************************)
let rec semant_expr expr env =

  (* Checks that argument types match formal var types *)
  let check_arg_type formal_param arg_expr =
    let (actual_type, arg_expr') = semant_expr arg_expr env
    and expected_type = fst formal_param in
    let msg = Printf.sprintf "%s, expected typ %s, but got %s"
        (string_of_expr (arg_expr)) (string_of_typ expected_type) (string_of_typ actual_type)
    in
    ignore(check_matching_types expected_type actual_type (ArgumentTypeMismatch(msg)));
    (actual_type, arg_expr')
  in

  match expr with
    ILiteral i  -> (Int, SILiteral i)
  | Fliteral f  -> (Float, SFliteral f)
  | BoolLit b   -> (Bool, SBoolLit b)
  | StringLit s -> (String, SStringLit s)
  | Noexpr      -> (Void, SNoexpr)

  | Binop (e1, op, e2) as ex ->
    (* Binary operations work with operands of the same type *)
    let (typ1, e1') = semant_expr e1 env
    and (typ2, e2') = semant_expr e2 env in
    let same_type = typ1 = typ2 in
    let end_typ = match op with
        Add | Sub | Mult | Div when same_type && typ1 = Int -> Int
      | Add | Sub | Mult | Div when (typ1 = Float || typ1 = Int) && (typ2 = Float || typ2 = Int) -> Float
      | Equal | Neq  when same_type -> Bool
      | Less | Greater when same_type && (typ1 = Float || typ1 = Int) && (typ2 = Float || typ2 = Int) -> Bool
      | And | Or when same_type && typ1 = Bool -> Bool
      | Concat when (typ1 = String && (typ2 = String || typ2 == Int || typ2 == Float)) ||
                    (typ2 = String && (typ1 == Int || typ1 == Float)) -> String
      | _  ->
        let msg = Printf.sprintf "unexpected types in binary op (%s and %s): %s"
              (string_of_typ typ1)  (string_of_typ typ2) (string_of_expr ex)
        in raise (IllegalBinaryOp(msg))
    in (end_typ, SBinop((typ1, e1'), op, (typ2, e2')))

  | Unop (op, e) as ex ->
    (* Only one type of Unop supported *)
    let (vtype, e') = semant_expr e env in
    if vtype == Bool then (vtype, SUnop(op, (vtype, e')))
    else raise (IllegalUnaryOp (string_of_expr ex))

  | Id id ->
    (* if we are in a class, adjust the ID to be a class access, of a "<ClassName>*" parameter *)
    let typ = find_type_of_id env.symbols id in
      if env.in_class then
        let mangled_name =
          if env.constructor then env.obj_name
          else mangled_obj_varname env.class_name
        in
        if List.mem id (instance_variables_of_cls env.class_name) then
          (typ, SClassAccess(Obtype(env.class_name), (typ, SId(mangled_name)), id))
        else (typ, SId id)
      else (typ, SId id)

  | Assign (v, e) as ex ->
    (* Check that the expr produces the same type as the variable it is assigned to *)
    let _ =
      (match v with
        Id (id) -> id
      | _ -> raise (VariableAssignmentError("assignment is performed on a variable")))
    in
    let (var_typ, id) = semant_expr v env
    and (ret_type, e') = semant_expr e env in
    let err =
      let msg = Printf.sprintf "%s. expected %s, got %s here: %s"
            assignment_typ_mismatch (string_of_typ var_typ) (string_of_typ ret_type) (string_of_expr ex)
      in VariableAssignmentError(msg)
    in (check_matching_types var_typ ret_type err, SAssign((var_typ, id), (ret_type, e')))

  | FunctionCall (fname, args) ->
    (* 1. Make sure function exists *)
    let func = find_function fname in

    (* 2. Check that param length is equal to the num args provided *)
    if List.length args != List.length func.formals then
      raise (FunctionArgumentLengthMismatch (func_arg_num_mismatch ^ fname))
    else
    (* 3. Check that the arguments passed are of the expected type *)
      let args' =
        (match fname with
        (* this is a work around for handling the built in print function, which can accept multiple types *)
        | "Meow" ->
          (match args with
            arg :: []  -> [semant_expr arg env]
          | _ -> raise (FunctionArgumentLengthMismatch("built in string function takes 1 argument only\n")))
        | "Scan" ->
          (match args with
            arg :: []  ->
            let (typ, arg') = semant_expr arg env in
            (match arg with
              Id(_) when typ = String -> [(typ, arg')]
            | _ -> raise (ArgumentTypeMismatch(scan_error)))
          | _ -> raise (FunctionArgumentLengthMismatch(scan_error)))
        | _ -> List.map2 check_arg_type func.formals args)
      in
      (func.typ, SFunctionCall(fname, args'))

  | MethodCall (obj_expr, meth_name, args) as ex ->
    (* 1. Check that the object exists in the symbol table  *)
    let (v_type, obj_expr') =
      match obj_expr with
        Id("this") ->
          if env.in_class = false then
            let msg = Printf.sprintf "%s, but found in func %s"
                use_of_this_outside_class env.function_name
            in raise (InvalidMethodCall(msg))
          else (Obtype(env.class_name), SId(mangled_obj_varname env.class_name))
      (* call method on variable for object, or array element that is object only *)
      | Id(_) | ArrayAccess(_, _) -> semant_expr obj_expr env
      | _ ->
          let msg = Printf.sprintf "%s, found %s"
            method_call_expr (string_of_expr ex)
          in raise(InvalidMethodCall(msg))
    in

    (* 2. Check that the method exists within the class and get it *)
    let cname =
      match v_type with
      | Obtype object_type -> object_type
      | _ ->
          let msg = Printf.sprintf "%s found %s instead of Objtype in %s"
            invalid_method_call (string_of_typ v_type) (string_of_expr ex)
          in raise (InvalidMethodCall(msg))
    in
    let meth = find_class_method cname meth_name in

    (* 3. Check that param length is equal to the num args provided *)
    if List.length args != List.length meth.formals then
      let msg = Printf.sprintf "%s %s (got %s, expected %s)"
            meth_arg_num_mismatch  meth_name (string_of_int (List.length args))
            (string_of_int (List.length meth.formals))
      in raise (MethodArgumentLengthMismatch(msg))
    else
    (* 4. Check that the arguments passed are of the expected type *)
      let args' =
        try List.map2 check_arg_type meth.formals args
        with ArgumentTypeMismatch(s) ->
          let msg = Printf.sprintf "method %s received arg of unexpected type: %s" meth_name s
          in raise(ArgumentTypeMismatch(msg))
      in
      (* 5. Convert to function call; add the object from step 1 as the first argument *)
      (meth.typ, SFunctionCall(m_to_f_name cname meth_name, (v_type, obj_expr') :: args'))

  | NewArray (arr_name, arr_typ, arr_size, expr_list) as ex ->
    (* 1. Check to make sure that size is integer *)
    let array_size_typ =
      match arr_size with
        ILiteralArraySize i ->
          (* 2. check integer literal against expr_list length *)
          let num_items = List.length expr_list in
            if num_items > i then
              raise (ExcessArrayInput(excess_array_item ^ " " ^ string_of_int num_items))
            else if i < 1 then
              raise (InvalidArraySizeSpecified("size of array must be integer > 0"))
            else (); Int
      | VariableArraySize s -> find_type_of_id env.symbols s
    in
    if array_size_typ != Int then
      raise (InvalidArraySizeSpecified(invalid_array_size_msg ^ string_of_expr ex ))
    else
      let expr_list' = List.fold_left (fun acc e -> semant_expr e env :: acc) [] expr_list
      (* 3. if expr_list, check that the expressions match the content type of array *)
      (* This is also a good opportunity to "massage" these array expressions into   *)
      (* assignment statements after the array is allocated *)
      and massage_array_args idx (expr_typ, expr) =
        let msg = Printf.sprintf "%s, expected: %s, got: %s in %s"
          invalid_array_item_msg (string_of_typ arr_typ) (string_of_typ expr_typ) (string_of_expr ex)
        and result =
          (expr_typ, SAssign((arr_typ, SArrayAccess(arr_name, (Int, SILiteral(idx)))), (expr_typ, expr)))
        in
        match expr_typ with
          Obtype(_) when expr_typ == arr_typ -> result
        | _ when expr_typ = arr_typ -> result
        | _ -> raise (InvalidArrayItem(msg))
      in
      let array_constructor = List.mapi massage_array_args (List.rev expr_list') in

      (* make sure to add the allocated obj to symbol table so it can be referenced *)
      Hashtbl.add env.symbols arr_name (Arrtype(arr_size, arr_typ));
      (Arrtype(arr_size, arr_typ), SNewArray(arr_name, arr_typ, arr_size, array_constructor))

  | NewInstance (obj_name, typ, expr_list) as ex ->
    (* 1. You can only create an "instance" of something that is type Objtype *)
    let (cname, cls) =
      match typ with
        (* 2. Check that the class of new instance actually exists - valid *)
        Obtype o -> (o, find_class o)
      | _ ->
          let msg = Printf.sprintf "%s, found: %s" invalid_object_creation (string_of_expr ex)
          in raise (ObjectCreationInvalid(msg))
    in

    (**************************************************************************)
    (* Modifies the list of expressions provided upon creation of a new class *)
    (* instance to incorporate both default and explicit values for instance  *)
    (* variables. This approach, namely creating a new env, allows for the    *)
    (* following in a default constructor, where two instance vars can build  *)
    (* the value of a third. The main strategy is to build up semantic expr   *)
    (* for the defaults, then append custom args. This allows the LLVM code   *)
    (* to always set the defaults, then override with the custom, if necessary*)
    (*                                                                        *)
    (* HAI ITZ ME CLASS Example                                               *)
    (*       ITZ ME NUMBR num1 IZ 2.                                          *)
    (*        ITZ ME NUMBR num2 IZ 5.                                         *)
    (*       ITZ ME NUMBR sum IZ SUM OF num1 AN num2.                         *)
    (*        ...                                                             *)
    (*    KBYE                                                                *)
    (**************************************************************************)
    let mangled_constructor_args =
      let construct_env = {
        (* Create a new environment representing constructor *)
        in_class = true;  (* turns on auto ClassAccess for instance vars *)
        class_name = cname;
        constructor = true; (* tells us we are working on a constructor scenario *)
        obj_name = obj_name;
        function_name = "";
        returns = Void;
        symbols = Hashtbl.create 10;
      } in
      (* Create a list of assignment expressions for default args *)
      let default_vars =
        let set_default_vars acc (typ, id, expr) =
          let (typ', e') = semant_expr expr construct_env in
          (match e' with
              SNoexpr -> acc
            | _ ->
              let lhs = (typ, SClassAccess(Obtype(cname), (Obtype(cname), SId(obj_name)), id))
              and rhs = (typ', e') in
              Hashtbl.add construct_env.symbols id typ;
              (typ, SAssign(lhs, rhs)) :: acc)
        in
        List.fold_left set_default_vars [] (List.rev cls.cvars) in

      (* This tricky code is meant to allow someone to assign instance vars
        by name, using assignment-like expressions, to create a new class instance *)
      let check_constructor_arg acc expr =
        (* make sure that all variables/types in assignment are part of class *)
        (match expr with
          | Assign(Id(id), e) ->
              let (typ, e') = semant_expr e env in
              let cvars = List.map (fun (typ, name, _) -> (typ, name)) cls.cvars in
              if List.mem (typ, id) cvars then
                (* Convert the assignment statement into an assignment with class access *)
                let lhs = (typ, SClassAccess(Obtype(cname), (Obtype(cname), SId(obj_name)), id))
                and rhs = (typ, e') in
                (* Append the new expression to the growing list *)
                (typ, SAssign(lhs, rhs)) :: acc
              else
                let msg = Printf.sprintf "%s, found: %s in allocation of new %s"
                          object_constructor_types (string_of_expr expr) cname
                in raise (ObjectConstructorInvalid(msg))
          | _ ->
            let msg = Printf.sprintf "%s, found: %s in allocation of new %s"
                    object_constructor_error (string_of_expr expr) cname
            in raise(ObjectConstructorInvalid(msg)))
        in
      List.fold_left check_constructor_arg default_vars expr_list in

    (* Make sure to add the allocated obj to symbol table so it can be referenced *)
    Hashtbl.add env.symbols obj_name (Obtype (cname));
    (* Return SAST for new instance with modified constructor! *)
    (typ, SNewInstance(obj_name, typ, List.rev mangled_constructor_args))

  | ClassAccess (v, class_var) as ex ->
    (* 1. Check that the object exists in the symbol table  *)
    let obj_name =
      (match v with
          Id (id) -> id
        | _ -> raise (InstanceVariableAccessInvalid(class_access_msg)))
    in
    let (typ, identifer) = semant_expr v env in
    (* 2. You can only "access" instance variables of type Obtype *)
    let cname =
      (match typ with
          Obtype o -> o
        | _ ->
          let msg = Printf.sprintf "%s, found: %s" invalid_instance_var_access (string_of_expr ex)
          in raise (InstanceVariableAccessInvalid(msg)))
    in
    (* 3. Check that the instance variable exists within the class *)
    let cls = find_class cname in
    let cvars = List.map (fun (_, name, _) -> name) cls.cvars in
    if List.mem class_var cvars then
      let rec find_typ n vars =
        match vars with
          [] -> raise (InternalError("unexpectedly could not determine type of class variable\n"))
        | (typ, name, _) :: t -> if name = n then typ else find_typ n t
      in
      (find_typ class_var cls.cvars, (SClassAccess(Obtype(cname), (typ, identifer), class_var)))
    else
      let msg = Printf.sprintf "%s, instance of class %s, has no member %s"
                obj_name cname class_var
      in raise (InstanceVariableNotFound(msg))

  | ArrayAccess (array_id, e) as ex ->
    (* 1. Check that the array exists in the symbol table  *)
    let typ = find_type_of_id env.symbols array_id in

    (* 2. You can only have index access to items of type ArrType *)
    let (_, contents_typ) =
      match typ with
      | Arrtype (ILiteralArraySize(sz), arr_typ) ->
        (match e with
            (* Test if array access is out of bounds; note this is only possible
                to do when the array hasn't been passed as a parameter, in which i < 0 *)
            ILiteral i ->
              if ( sz > 0 && i >= sz ) || i < 0 then
                let msg = Printf.sprintf "%s, using index %s in %s, an array of size %s"
                          array_access_out_of_bounds (string_of_int i) array_id (string_of_int sz)
                in raise (InvalidArrayAccess(msg))
              else (sz, arr_typ)
          | _ -> (sz, arr_typ))
      | Arrtype (VariableArraySize(_), arr_typ) -> (0, arr_typ) (* this 0 is to make the compiler happy *)
      | _ ->
        let msg = Printf.sprintf "%s; attempting index on '%s' of type %s"
                  array_access_array_only array_id (string_of_typ typ)
        in raise (InvalidArrayAccess(msg))
    in
    (* 3. Check to make sure that the array is going to be indexed by an integer typ *)
    let (typ', e') = semant_expr e env in
    (match typ' with
      Int -> (contents_typ, SArrayAccess ((array_id), (typ', e')))
    | _ ->
      let msg = Printf.sprintf "%s found index expression %s of type %s"
                array_access_integer (string_of_expr ex) (string_of_typ typ')
      in raise (InvalidArrayAccess(msg)))

(****************************************)
(* Main function for checking semantics *)
(* of statements                        *)
(****************************************)
let rec semant_stmt stmt env =

  (* if/else branching: checks that termination expr is a Boolean *)
  let check_bool_expr e =
    let (t', e') = semant_expr e env in
      if t' == Bool then (t', e')
      else
        let msg = Printf.sprintf "%s %s %s"
          op_type_mismatch_boolean (string_of_typ t') (string_of_expr e)
        in raise (ControlFlowIllegalArgument(msg))
  and

  (* for looping: checks that op is Increment or Decrement *)
  check_control_op op =
    match op with
      Increment -> op
    | Decrement -> op
    | _ -> raise (ControlFlowIllegalArgument(op_type_mismatch_inc_dec ^ string_of_op op))
  and

  (* for looping: checks that index is an Integer *)
  check_control_index e =
    let (t', e') = semant_expr e env in
      if t' == Int then (t', e')
      else
        let msg = Printf.sprintf "%s %s %s"
          op_type_mismatch_int (string_of_typ t') (string_of_expr e)
        in raise (ControlFlowIllegalArgument(msg))
  and

  (* for looping: second expr is optional or index assignment *)
  check_index_assignment e =
    let (t', e') = semant_expr e env in
    match e' with
      SNoexpr -> (t', e')
    | SAssign(_, _) -> (t', e')
    | _ ->
      let msg = Printf.sprintf "index assignment expected in expression: %s" (string_of_expr e)
      in raise (ControlFlowIllegalArgument(msg))
  and

  (* for looping: checks that loop termination is a binary operation of < or > *)
  check_loop_termination e =
    let (t', e') = semant_expr e env in
      match e' with
        SBinop(_, op, _) ->
          (match op with
            Less | Greater | Equal | Neq -> (t', e')
          | _ ->
            let msg = Printf.sprintf "%s %s" op_type_mismatch_loop_term (string_of_expr e)
            in raise (ControlFlowIllegalArgument(msg)))
      | _ ->
        let msg = Printf.sprintf "%s expected binary operation in loop: %s"
          expr_type_mismatch (string_of_expr e)
        in raise (ControlFlowIllegalArgument(msg))
  in

  match stmt with
    Expr e    -> SExpr(semant_expr e env)

  | Return e  ->
    if env.returns = Void then
        let msg = Printf.sprintf "%s; see function %s" return_from_void_func env.function_name
        in raise (ReturnFromVoidFunction(msg))
    else
        let (typ, e') = semant_expr e env in
        let msg = Printf.sprintf "%s; expected: %s, got: %s in function %s"
          return_type_invalid (string_of_typ env.returns) (string_of_typ typ) env.function_name
        in
        ignore(check_matching_types typ env.returns (ReturnTypeInvalid(msg)));
        SReturn((typ, e'))

  | If (e, stmt1, stmt2) ->
    SIf(check_bool_expr e,
    semant_stmt stmt1 env,
    semant_stmt stmt2 env)

  | For (op, e1, e2, e3, stmt) ->
    SFor(check_control_op op,         (* increment or decrement *)
        check_control_index e1,       (* index *)
        check_index_assignment e2,    (* optional index assignment *)
        check_loop_termination e3,    (* termination condition *)
        semant_stmt stmt env)         (* loop body *)

  | Block b ->
    let rec check_stmt_list = function
        [Return _ as s] -> [semant_stmt s env]
      | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
      | s :: ss         ->
        let fst = semant_stmt s env
        and lst = check_stmt_list ss in
        fst :: lst
      | []              -> []
    in SBlock(check_stmt_list b)

  | Dealloc (id) as s ->
    (* Check that the dealloced item is of type ObjType or ArrType *)
    let id =
      (match id with
        Id(v) -> v
        | _ -> raise (InvalidDealloc(invalid_deallocation_msg ^ string_of_stmt s)))
    in
    let typ = find_type_of_id env.symbols id in
    (match typ with
      Obtype _ | Arrtype _ | String -> SDealloc(typ, SId(id))
    | _ ->
      let msg = Printf.sprintf "%s %s is of typ %s"
        invalid_deallocation_msg id (string_of_typ typ)
      in raise (InvalidDealloc(msg)))

  | ClassAssign (id, instance_var, e) ->
    (* 1. id must correspond to an ObjType *)
    let (typ, identifier) = semant_expr id env in
    (match typ with
      Obtype (cname) ->
        let cls = find_class cname in
        let cvars = List.map (fun (typ, name, _) -> (typ, name)) cls.cvars in
        let (vtype, e') = semant_expr e env in

        (* 2. the instance variable must exist in the class and the
        item being assigned must be of the correct type *)
        if List.mem (vtype, instance_var) cvars then
          SClassAssign(Obtype(cname), (typ, identifier), instance_var, (vtype, e'))
        else
          let msg = Printf.sprintf "%s %s to %s.%s"
                invalid_cls_member_assign (string_of_typ typ) cname instance_var
          in raise (InvalidClassMemberAssignment(msg))
    | _ ->
      let msg = Printf.sprintf "%s %s is of type %s"
          member_assign_cls_only (string_of_expr id) (string_of_typ typ)
      in raise (InvalidClassMemberAssignment(msg)))

  | ArrayAssign (id, idx_e, e) ->
    (* 1. make sure that the variable is an array type *)
    let typ = find_type_of_id env.symbols id in
    (match typ with
      Arrtype (_, ty) ->
        (* 2. make sure that the idx_e expression yields an integer *)
        let (idx_typ, idx_e') = semant_expr idx_e env in
        (match idx_typ with
          Int ->
            (* 3. make sure that the expression type being assigned matches array content type *)
            let (exp_typ, e') = semant_expr e env in
              if exp_typ = ty then
                SArrayAssign(id, (idx_typ, idx_e'), (exp_typ, e'))
              else
                let msg = Printf.sprintf "%s, found '%s' of type %s"
                          invalid_array_item_msg (string_of_expr e) (string_of_typ exp_typ)
                in raise (InvalidArrayAssignment(msg))
          | _ ->
            let msg = Printf.sprintf "%s found index expression '%s' of type %s"
                    array_access_integer (string_of_expr idx_e) (string_of_typ idx_typ)
            in raise (InvalidArrayAssignment(msg)))
    | _ ->
      let msg = Printf.sprintf "%s; %s is not an array" array_access_array_only id
      in raise (InvalidArrayAssignment(msg)))


(****************************************)
(* Checks that a function body is       *)
(* semantically correct.                *)
(****************************************)
let check_function_body func env =
  (*
    1. Build local symbol table of variables for this scope
  *)
  List.iter (fun (typ, name) -> Hashtbl.add env.symbols name typ) func.formals;
  List.iter (fun (typ, name, _) -> Hashtbl.add env.symbols name typ) func.locals;

  (*
    2. Refactoring step: moves assignments in func.locals into function body
       beginning analogous to make the following change in a c function :
          int value = 2;   -> int value;
                              value = 2;
  *)
  let create_assignment_stmt build local_var_bind =
    (match local_var_bind with
      (_, _, Noexpr)  -> build
    | (_, name, expr) -> Expr(Assign(Id(name), expr)) :: build) in
  let new_assignments =  List.fold_left create_assignment_stmt [] func.locals in
  let adjusted_body = List.rev new_assignments @ func.body in

  (* 3. Build up the SAST Tree for the Function *)
  semant_stmt (Block adjusted_body) env

(****************************************************)
(* Main function for checking to ensure contents of *)
(* function/method is semantically valid.           *)
(****************************************************)
let check_function_or_method func env =
  (* 1. Get a list of formal names and local variable names then
     2. Check for duplicate formal and duplicate local variable names on their own
     3. Check for duplicates in formals and locals together
     4. If there are other variables in env (i.e., instance vars) also check those *)
  let list_formal_names = List.fold_left (fun acc (_, name) -> name :: acc) [] func.formals
  and list_locals_names = List.fold_left (fun acc (_, name, _) -> name  :: acc) [] func.locals
  and list_other_names = Hashtbl.fold (fun k _ acc -> k :: acc) env.symbols []
  in
  find_duplicate (list_formal_names) dup_formal_msg;
  find_duplicate (list_locals_names) dup_local_var_msg;
  find_duplicate (list_formal_names @ list_locals_names) dup_form_local_msg;
  find_duplicate (list_formal_names @ list_locals_names @ list_other_names) dup_form_instance_msg;

  (* 4. Step to make LLVM code happy; main function must be 'main' not 'Main' *)
  let adjusted_function_name f = if f.fname = "Main" then "main" else f.fname
  in env.function_name <- func.fname;

  (* 5. Check contents of function body, producting SAST version *)
  let checked_func = {
    styp = func.typ;
    sfname = adjusted_function_name func;
    sformals = func.formals;
    slocals  = func.locals;
    sbody = match check_function_body func env with
        SBlock(stmt_list) -> stmt_list
      | _ -> raise (InternalError ("unexpected_error: no block in function body"))
  } in
  checked_func

(****************************************)
(* Checks that class definition is      *)
(* semantically correct.                *)
(****************************************)
 let check_class cls =
  (* instance variables defined cannot be duplicated within a single cls *)
  let list_instance_vars = List.fold_left (fun acc (_, name, _) -> name  :: acc) [] cls.cvars
  in
  find_duplicate (list_instance_vars) dup_instance_var_msg;

  (* create a new environment for the class, will contain instance vars *)
  let env = {
    in_class = true;
    class_name = cls.cname;
    constructor = false;
    obj_name = "";
    function_name = "";
    returns = Void;
    symbols = Hashtbl.create 10;
  } in

  (* Make sure that default values for instance variables make sense *)
  let eval_instance_var (typ, name, expr) =
    (match expr with
      (* no default value for instance var provided *)
      Noexpr -> Hashtbl.add env.symbols name typ; (typ, name, (Void, SNoexpr))
    | _ ->
      (* check assignment statement types *)
      let (typ', expr') = semant_expr expr env in
      if typ != typ' then
        let msg = Printf.sprintf "%s %s, expected %s"
          object_constructor_types (string_of_typ typ') (string_of_typ typ)
        in raise (ObjectInstanceVariableInvalid(msg))
      else
        Hashtbl.add env.symbols name typ; (typ, name, (typ', expr')))
  in
  let instance_vars_evaluated = List.map eval_instance_var (List.rev cls.cvars) in

  (* check the methods in each class, passing a symbol tbl
     that includes the instance vars *)
  let checked_cls = {
    scname = cls.cname;
    scvars = instance_vars_evaluated;
    scfuncs = List.map (
      fun f ->
        env.returns <- f.typ;
        check_function_or_method f env
    ) cls.cfuncs;
  } in
  checked_cls

(****************************************************)
(* Wrapper for function that checks of function is  *)
(* semantically correct.                            *)
(****************************************************)
let check_function func =

  (* create a new environment for the function scope *)
  let env = {
    in_class = false;
    class_name = "";
    constructor = false;
    obj_name = "";
    returns = func.typ;
    function_name = func.fname;
    symbols = Hashtbl.create 10;
  }
  in check_function_or_method func env

(******************************************************)
(* Moves a method within a class into the global      *)
(* space by renaming it "<Class_Name>.<MethodName>"   *)
(* and providing the Objtype(Class_Name) as the first *)
(* argument, an argument named "<Class_Name>*"        *)
(******************************************************)
let lift_methods_to_global_space cls =
  let lift_method m =
    {
      styp = m.styp;
      sfname = m_to_f_name cls.scname m.sfname;
      sformals = (Obtype(cls.scname), mangled_obj_varname cls.scname) :: m.sformals;
      slocals  = m.slocals;
      sbody = m.sbody;
    }
  in
  List.map lift_method cls.scfuncs

(******************************************************************************)
(* Entry point for Semantic Checker, transforming AST to SAST                 *)
(******************************************************************************)
let check (_, functions, classes) =

  (* 1. add built in functions to list of functions and
     2. Check for any duplicate function, method and class names *)
  let functions' = add_built_ins functions in
  check_duplicates functions' classes;

  (* 3. Since functions/classes are global, create maps of functions, classes *)
  List.iter (fun func -> Hashtbl.add function_tbl func.fname func) functions';
  List.iter (fun cls-> Hashtbl.add class_tbl cls.cname cls) classes;

  (* 4. Make sure that a "main" function exists, and if so, continue with
       creating a list of checked functions, converted to SAST form *)
  if Hashtbl.mem function_tbl "Main" then
    let semant_classes = List.map check_class classes
    and semant_funcs = List.map check_function functions in

    (* 5. The combined functions represent the "lifted" class methods and usual functions *)
    let combined_functions =
      List.fold_left (
        fun fs cls -> lift_methods_to_global_space cls @ fs
      ) semant_funcs semant_classes
    in
    ([], combined_functions, semant_classes)

  else raise (MissingMainFunction (missing_main_func_msg))
