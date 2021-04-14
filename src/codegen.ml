(******************************************************************************)
(* Code generation: translate takes a semantically checked AST and produces   *)
(* LLVM IR. Note: This code significantly inspired by codegen.ml of the       *)
(* MicroC Compiler by S Edwards (PLT, Spring 2021)                            *)
(******************************************************************************)
module L = Llvm
module A = Ast
open Exceptions
open Sast

module StringMap = Map.Make(String)

(* Hash table of structs representing user-defined classes *)
let struct_types:(string, L.lltype) Hashtbl.t = Hashtbl.create 10
let struct_field_idx:(string, int) Hashtbl.t = Hashtbl.create 10
let local_variables:(string,  L.llvalue) Hashtbl.t = Hashtbl.create 10
let global_functions:(string, (L.llvalue * sfunc_decl)) Hashtbl.t = Hashtbl.create 10

(* Finds a struct by its original class name *)
let find_struct_by_cls cls_name =
  try Hashtbl.find struct_types cls_name
  with | Not_found -> raise (UnknownStruct("struct for class " ^ cls_name ^ "unknown"))

(* Return the value for a variable, else raise error *)
let lookup_variable var_name env =
  try Hashtbl.find env var_name
  with _ -> raise (VariableNotFound("codegen error: " ^ undeclared_msg ^ "variable: " ^ var_name))

(* Return the value for a function name or argument *)
let lookup_function func_name =
  try Hashtbl.find global_functions func_name
  with _ -> raise (FunctionNotFound("codegen error: " ^ undeclared_msg ^  "function: " ^ func_name))

let lookup_index cls_name field_name =
  try Hashtbl.find struct_field_idx (cls_name ^ "." ^ field_name)
  with _ -> raise (InstanceVariableNotFound("codegen error: " ^ cls_name ^ "." ^ field_name))

let context    = L.global_context ()
let the_module = L.create_module context "Meowlang"

(* Easier to not have to rewrite these types*)
let i1_t      = L.i1_type     context
let i8_t      = L.i8_type     context
let i32_t     = L.i32_type    context
let float_t   = L.double_type context
let void_t    = L.void_type   context
let str_t     = L.pointer_type i8_t

(* Finds the LLVM type corresponding to the Meowlang type *)
let rec ltype_of_typ = function
    A.Int           -> i32_t
  | A.Bool          -> i1_t
  | A.Float         -> float_t
  | A.Void          -> void_t
  | A.String        -> str_t
  | A.Obtype(c)     -> L.pointer_type (find_struct_by_cls c)
  | A.Arrtype(_, t) -> L.pointer_type (ltype_of_typ t)

(* Creates a function prototype *)
let create_func_prototype fdecl =
  let name =  fdecl.sfname
  and return_typ = ltype_of_typ fdecl.styp
  and formal_types =
    Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
  in
  let ftype = L.function_type return_typ formal_types in
  L.define_function name ftype the_module, fdecl

(* Declare and setup required hash tables for a struct based on an object *)
let codegen_struct cls =
  (* save new struct to known struct list *)
  let struct_t = L.named_struct_type context cls.scname in
  Hashtbl.add struct_types cls.scname struct_t;

  let var_name_list = List.map (fun (_, n, _) -> n) cls.scvars
  and inst_var_ltyps = List.map (fun (t, _, _) -> ltype_of_typ t) cls.scvars in
  List.iteri (
    fun idx name -> Hashtbl.add struct_field_idx (cls.scname ^ "." ^ name) idx
  ) var_name_list;
  L.struct_set_body struct_t (Array.of_list inst_var_ltyps) false

(* Provides the c-equivalent formatting string for a given expr *)
let format (typ, _) =
  match typ with
    A.Int    -> "%d\n"
  | A.Float  -> "%g\n"
  | A.String -> "%s\n"
  | A.Bool   -> "%d\n"
  | _ -> raise (NotYetSupported("formatting for type not yet supported"))

  let add_local (typ, local_name, _) env builder =
    let new_local = L.build_alloca (ltype_of_typ typ) local_name builder
    in Hashtbl.add env local_name new_local

(*********************************************************)
(* Build Function: Fill in the body of the each function *)
(*********************************************************)
let build_function fdecl =

  (* Create the prototypes for build in functions (I/O and casting) *)
  let printf_func =
    let printf_t = L.var_arg_function_type i32_t [| str_t |] in
    L.declare_function "printf" printf_t the_module in

  let scanf_func =
    let scanf_t = L.function_type i32_t [| L.pointer_type str_t |] in
    L.declare_function "custom_scanf" scanf_t the_module in

  let atoi_func =
    let atoi_t = L.function_type i32_t [| str_t |] in
    L.declare_function "atoi" atoi_t the_module in

  let itoa_func =
    let itoa_t = L.function_type str_t [| i32_t |] in
    L.declare_function "custom_itoa" itoa_t the_module in

  let strcmp_func =
    let strcmp_t = L.function_type i32_t [| str_t ; str_t |] in
    L.declare_function "custom_strcmp" strcmp_t the_module in

  let (the_function, _) = Hashtbl.find global_functions fdecl.sfname in
  let builder = L.builder_at_end context (L.entry_block the_function) in

  (* Construct the functions locals (formal + local vars) *)
  let add_formal acc (typ, formal_name) param =
    (* set name of param value to corresponding formal name *)
    L.set_value_name formal_name param;

    (* allocate space stack for formal *)
    let new_formal = L.build_alloca (ltype_of_typ typ) formal_name builder in
    ignore (L.build_store param new_formal builder);  (* store %param %new_formal *)
    Hashtbl.add local_variables formal_name new_formal;
    (formal_name, new_formal) :: acc (* to make compiler happy *)
  in

  (* Clear existing locals and start over *)
  Hashtbl.clear local_variables;
  let params = (Array.to_list (L.params the_function)) in
  ignore(List.fold_left2 add_formal [] fdecl.sformals params);
  List.iter (fun l -> add_local l local_variables builder) fdecl.slocals;

  (* Formatting expressions, for printf-ing *)
  let format_str fmt = L.build_global_stringptr fmt "fmt" builder in

  (************************************************)
  (* Helper function for building up expressions  *)
  (************************************************)
  let rec expr builder ((_, e) : sexpr) env =
    match e with
      SILiteral i  -> L.const_int i32_t i
    | SBoolLit b   -> L.const_int i1_t (if b then 1 else 0)
    | SFliteral l  -> L.const_float_of_string float_t l
    | SStringLit s -> L.build_global_stringptr s "str" builder
    | SId var      -> L.build_load (lookup_variable var env) var builder
    | SNoexpr      -> L.const_int i32_t 0

    | SCast(t, (typ, e))  ->
      let rhs = expr builder (typ, e) env
      and llvm_typ = ltype_of_typ t in
      (match typ with
        A.Float when t = A.Int -> L.build_fptosi rhs llvm_typ "cast_v" builder
      | A.Int when t = A.Float -> L.build_uitofp rhs llvm_typ "cast_v" builder
      | A.String when t = A.Int ->
          L.build_call atoi_func [| rhs |] "atoi_call" builder
      | A.Int when t = A.String ->
          L.build_call itoa_func [| rhs |] "itoa_call" builder
      | _ -> raise (NotYetSupported("codegen: cast operation not yet supported")))

    | SAssign ((_, lhs), e)   ->
      let rhs = expr builder e env in
      let lhs =
        match lhs with
        (* Used in assigning variables; not used in assigning class members *)
          SId(var) -> lookup_variable var env
        | SClassAccess(A.Obtype(cname), ((_, SId(_)) as v), inst_v) ->
            let index = lookup_index cname inst_v
            and load_tmp = expr builder v env in
            L.build_struct_gep load_tmp index "tmp" builder
        | SArrayAccess(v, e) ->
            let index_args = [|expr builder e env|] in
            let arr = L.build_load (lookup_variable v env) "arr_ptr" builder in
            L.build_gep arr index_args "element_ptr" builder

        | _ -> raise (NotYetSupported("codegen: assignment op not yet supported"))
      in ignore(L.build_store rhs lhs builder); rhs

    | SUnop(_, ((_, _) as e)) ->
      let e' = expr builder e env in
      L.build_not e' "tmp" builder

    (* Binary operation between two integers *)
    | SBinop(((A.Int, _) as e1), op, ((A.Int, _) as e2)) ->
      let lhs = expr builder e1 env
      and rhs = expr builder e2 env in
      (match op with
        A.Add       -> L.build_add
      | A.Sub       -> L.build_sub
      | A.Mult      -> L.build_mul
      | A.Div       -> L.build_sdiv
      | A.Equal     -> L.build_icmp L.Icmp.Eq
      | A.Neq       -> L.build_icmp L.Icmp.Ne
      | A.Less      -> L.build_icmp L.Icmp.Slt
      | A.Greater   -> L.build_icmp L.Icmp.Sgt
      | A.Increment -> L.build_add
      | A.Decrement -> L.build_sub
      | _         ->
        let msg = "found binary operation not supported for two integers"
        in raise (NotYetSupported(msg))
      ) lhs rhs "binop_int_tmp" builder

    | SBinop(((A.String, _) as e1), A.Equal, ((A.String, _) as e2)) ->
      let lhs = expr builder e1 env
      and rhs = expr builder e2 env in
      L.build_call strcmp_func [| lhs ; rhs |] "stcmp_call" builder

    (* Binary operation between one or more floats *)
    | SBinop(((A.Float as t), (_ as v1)), op, ((A.Int as o), (_ as v2)))
    | SBinop(((A.Int as t), (_ as v1)), op, ((A.Float as o, (_ as v2))))
    | SBinop(((A.Float as t), (_ as v1)), op, ((A.Float as o), (_ as v2))) ->
      let build_cast v =
        L.build_uitofp v float_t "cast_v" builder
      in
      let lhs =
        let tmp_l = expr builder (A.Float, v1) env in
        if t = A.Int then build_cast tmp_l
        else tmp_l
      and rhs =
        let tmp_r = expr builder (A.Float, v2) env in
        if o = A.Int then build_cast tmp_r
        else tmp_r
      in
      (match op with
        A.Add     -> L.build_fadd
      | A.Sub     -> L.build_fsub
      | A.Mult    -> L.build_fmul
      | A.Div     -> L.build_fdiv
      | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
      | A.Neq     -> L.build_fcmp L.Fcmp.One
      | A.Less    -> L.build_fcmp L.Fcmp.Olt
      | A.Greater -> L.build_fcmp L.Fcmp.Ogt
      | _         ->
        let msg = "found binary operation not supported for one or more floats"
      in raise (NotYetSupported(msg))
      ) lhs rhs "binop_float_tmp" builder

    (* Binary operation for two booleans *)
    | SBinop(((A.Bool, _) as e1), op, ((A.Bool, _) as e2)) ->
      let lhs = expr builder e1 env
      and rhs = expr builder e2 env in
      (match op with
          A.Or    -> L.build_or
        | A.And   -> L.build_and
        | A.Neq   -> L.build_icmp L.Icmp.Ne
        | A.Equal -> L.build_icmp L.Icmp.Eq
        | _      ->
          let msg = "found binary operation not supported for two boolean values"
          in raise (NotYetSupported(msg))
      ) lhs rhs "binop_bool_tmp" builder

    (* Call to built in printf function *)
    | SFunctionCall ("Meow", [arg]) ->
      L.build_call printf_func [| format_str (format arg) ; (expr builder arg env) |] "printf" builder

    | SFunctionCall ("Scan", [(_, SId(arg))]) ->
      L.build_call scanf_func [| (lookup_variable arg env) |] "custom_scanf" builder

    (* Call a user-defined function *)
    | SFunctionCall (fname, args) ->
      let (fdef, fdecl) = lookup_function fname in
      let llargs = List.rev (List.map (fun a -> expr builder a env) (List.rev args))
      and result = (
        match fdecl.styp with
          A.Void -> ""
        | _ -> fname ^ "_result"
      ) in
      L.build_call fdef (Array.of_list llargs) result builder

    (* Create a new struct instance of class c with variable name n*)
    | SNewInstance(v, c, constructor_exprs) ->
      (match c with
        A.Obtype (cname) as cls -> (
        (* add new variable to local vars; a pointer to malloc'd item *)
        add_local (cls, v, None) env builder;
        (* assign the malloc to the new variable *)
        let rhs = L.build_malloc (find_struct_by_cls cname) "new_struct" builder
        and lhs = lookup_variable v env in
        ignore(L.build_store rhs lhs builder);

        (* now the tricky nonsense of the constructor; since we want the default args
        to be calculatable against each other, must build a separate symbol table *)
        let constructor_vars:(string, L.llvalue) Hashtbl.t = Hashtbl.create 10 in
        (* add all local entries... *)
        let _ = Hashtbl.iter (fun k v -> Hashtbl.add constructor_vars k v) local_variables in
        let build_constructor v (typ, e) =
          (* add it to a "constructor" symbol table *)
          (match e with
              SAssign((_, SClassAccess(_, (_, _), id)), _) ->
              ignore(add_local (typ, id, e) constructor_vars builder);
              (expr builder (typ, e) constructor_vars) :: v (* build it! *)
            | _ ->
              let msg = "codegen: class constructor pattern not yet supported"
              in raise (NotYetSupported(msg)))
        in
        ignore(List.fold_left build_constructor [] constructor_exprs); rhs
        )
      | _ -> raise (ObjectCreationInvalid("codegen: cannot create instance of anything but Obtype")))

    | SClassAccess(A.Obtype(cname), v, inst_v) ->
      let tmp_value = expr builder v env
      and index = lookup_index cname inst_v in
      let deref = L.build_struct_gep tmp_value index "tmp" builder in
      L.build_load deref "dr" builder

    | SNewArray (v, i_typ, n, setup_exprs) ->
      (* add new variable to local vars; a pointer to malloc'd item *)
      add_local (A.Arrtype(n, i_typ), v, None) env builder;
      (* determine value of n *)
      let n_val =
        match n with
          A.ILiteralArraySize i  -> L.const_int i32_t i
        | A.VariableArraySize id -> expr builder (A.Int, SId(id)) env
      and ar_typ = ltype_of_typ i_typ
      in
      let rhs = L.build_array_malloc ar_typ n_val "create_heap_array" builder
      and lhs = lookup_variable v env in
      ignore(L.build_store rhs lhs builder);

      (* now do the setup expressions, where the array contents are assigned *)
      (* this is like a constructor for a new array *)
      ignore(List.map (fun e -> expr builder e env) (List.rev setup_exprs)); rhs

    | SArrayAccess (v, e) ->
      let index_args = [|expr builder e env|]
      and arr = L.build_load (lookup_variable v env) "arr_ptr" builder in
      let gep = L.build_gep arr index_args "element_ptr" builder in
      L.build_load gep "array_entry" builder

    | _ -> raise (NotYetSupported("found expr or functions not yet supported"))
  in

  (* LLVM insists each basic block end with exactly one "terminator"
     instruction that transfers control. This function runs "instr builder"
     if the current block does not already have a terminator. *)
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder) in

  (*****************************************************)
  (* Helper function for building up statements        *)
  (* Returns the builder for the statement's successor *)
  (*****************************************************)
  let rec stmt builder = function
      SBlock sl   -> List.fold_left stmt builder sl
    | SExpr e     -> ignore(expr builder e local_variables); builder
    | SReturn e   -> ignore(
      match fdecl.styp with
        (* Special "return nothing" instr *)
        A.Void -> L.build_ret_void builder
        (* Build return statement *)
      | _ -> L.build_ret (expr builder e local_variables) builder
      ); builder

    | SIf (predicate, then_stmt, else_stmt) ->
      let bool_val = expr builder predicate local_variables in
      let merge_bb = L.append_block context "merge" the_function in
      let build_br_merge = L.build_br merge_bb in (* partial function *)

      let then_bb = L.append_block context "then" the_function in
      add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
      build_br_merge;

      let else_bb = L.append_block context "else" the_function in
      add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
      build_br_merge;

      ignore(L.build_cond_br bool_val then_bb else_bb builder);
      L.builder_at_end context merge_bb

    | SFor (inc_decrement, index, opt_index_assignment, termination_comparison, loop_body) ->
      (* perform the index assignment if it exists *)
      ignore(expr builder opt_index_assignment local_variables);

      let pred_bb = L.append_block context "for" the_function in
      ignore(L.build_br pred_bb builder);

      let body_bb = L.append_block context "for_body" the_function in
      add_terminal (stmt
        (L.builder_at_end context body_bb)
        (* create the SBinop expr for incrementing and decrementing *)
        (* append assigning index inc/decrement to the end of the loop body *)
        (SBlock [loop_body ; SExpr(A.Int, SAssign(index, (A.Int, SBinop(index, inc_decrement, (A.Int, SILiteral 1)))))])
      )
        (L.build_br pred_bb);

      let pred_builder = L.builder_at_end context pred_bb in
      let bool_val = expr pred_builder termination_comparison local_variables in

      let merge_bb = L.append_block context "merge" the_function in
      ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
      L.builder_at_end context merge_bb

    | SClassAssign (A.Obtype(cname), v, inst_v, e) ->
      let rhs = expr builder e local_variables
      and lhs =
        let index = lookup_index cname inst_v
        and load_tmp = expr builder v local_variables in
        L.build_struct_gep load_tmp index "tmp" builder
      in
      ignore(L.build_store rhs lhs builder); builder

    | SArrayAssign (v, idx_e, (t, assign_e)) ->
      (* Utilize code already created to handle assignment expressions *)
      let converted_expr =
        (t, SAssign((t, SArrayAccess(v, idx_e)), (t, assign_e)))
      in
      ignore(expr builder converted_expr local_variables); builder

    | SDealloc (v) ->
      let tmp_value = expr builder v local_variables in
      ignore(L.build_free (tmp_value) builder); builder

    | _ -> raise (NotYetSupported("complex stmts not yet supported"))
  in

  (* Build the code for each statement in the function *)
  let builder = stmt builder (SBlock fdecl.sbody) in

  (* Add a return if the last block falls off the end *)
  add_terminal builder (
    match fdecl.styp with
      A.Void -> L.build_ret_void
    | A.Float -> L.build_ret (L.const_float float_t 0.0)
    | t -> L.build_ret (L.const_int (ltype_of_typ t) 0)
  )

(***********************************************************************)
(* Translate : Takes a Sast.program and produces an Llvm.module        *)
(*                                                                     *)
(* Note that all "actionable" items in the program are now functions   *)
(* (there are no longer class methods/etc). Classes are useful because *)
(* they provide the struct definition corresponding to the class, but  *)
(* all class methods have been lifted to global scope in the semantic  *)
(* checking step.                                                      *)
(***********************************************************************)
let translate (_, functions, classes) =

  (* Create the relevant structs, based on the class definitions *)
  List.iter codegen_struct classes;

  (* Create each function's prototype *)
  (* this so we can call it even before we've created its body. *)
  (* this function builds up a map of function_name: prototype  *)
  (* Maps {function_name: (func_prototype, func_decl)} *)
  let add_functions_to_map fdecl =
    Hashtbl.add global_functions fdecl.sfname (create_func_prototype fdecl) in
  List.iter add_functions_to_map functions;

  (* Build each function *)
  List.iter build_function functions;
  the_module
