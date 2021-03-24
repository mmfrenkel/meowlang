(*
  Code generation: translate takes a semantically checked AST and produces LLVM IR

  Note: This code significantly inspired by codegen.ml of the MicroC Compiler by S Edwards
  (Programming Languages and Translators, Spring 2021)
*)

module L = Llvm
module A = Ast
open Exceptions
open Sast

module StringMap = Map.Make(String)

let context    = L.global_context ()
let the_module = L.create_module context "Meowlang"

(* Easier to not have to rewrite these types*)
let i32_t     = L.i32_type    context
let i1_t      = L.i1_type     context
let float_t   = L.double_type context
let void_t    = L.void_type   context
let str_t     = L.pointer_type  (L.i8_type  context)

(* Finds the LLVM type corresponding to the Meowlang type *)
let ltype_of_typ = function
    A.Int    -> i32_t
  | A.Bool   -> i1_t
  | A.Float  -> float_t
  | A.Void   -> void_t
  | A.String -> str_t
  | _ -> raise (NotYetSupported("complex types not yet supported"))
  (* | A.Arrtype(sz, typ) -> L.array_type (ltype_of_typ typ) sz *)

(* Creates a function prototype *)
let create_func_prototype fdecl =
    let name =  fdecl.sfname
    and return_typ = ltype_of_typ fdecl.styp
    and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
    in
    let ftype = L.function_type return_typ formal_types in
    L.define_function name ftype the_module, fdecl


(* Provides the c-equivalent formatting string for a given expr *)
let format (typ, _) =
  match typ with
    A.Int    -> "%d\n"
  | A.Float  -> "%g\n"
  | A.String -> "%s\n"
  | A.Bool   -> "%d\n"
  | _ -> raise (NotYetSupported("formatting for complex expr and functions not yet supported"))

(* Return the value for a variable, else raise error *)
let lookup_variable var_name map =
  try StringMap.find var_name map
  with _ -> raise (VariableNotFound("codegen error: " ^ undeclared_msg ^ var_name))

(* Return the value for a function name or argument *)
let lookup_function func_name map =
  try StringMap.find func_name map
  with _ -> raise (FunctionNotFound("codegen error: " ^ func_name))

(****************************************************************
  Translate : Takes a Sast.program and produces an Llvm.module
*****************************************************************)
let translate (_, functions, _) =

  (******* Create the prototype for printf/Meow, a built-in function ***********)
  let printf_t : L.lltype =
      L.var_arg_function_type i32_t [| str_t |] in
  let printf_func : L.llvalue =
      L.declare_function "printf" printf_t the_module in

  (****************** Create each function's prototype ************************)
  (* this so we can call it even before we've created its body. *)
  (* this function builds up a map of function_name: prototype  *)
  (* Maps {function_name: (func_prototype, func_decl)} *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
      let add_function_to_map m fdecl =
        StringMap.add fdecl.sfname (create_func_prototype fdecl) m in
    List.fold_left add_function_to_map StringMap.empty functions in

  (********** Build Function: Fill in the body of the each function ***********)
  let build_function fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (********* Construct the functions locals (formal + local vars) ***********)
    let add_formal map (typ, formal_name) param =
        (* set name of param value to corresponding formal name *)
        L.set_value_name formal_name param;

        (* allocate space stack for formal *)
        let new_formal = L.build_alloca (ltype_of_typ typ) formal_name builder in
        ignore (L.build_store param new_formal builder);  (* store %param %new_formal *)
        StringMap.add formal_name new_formal map

    and add_local map (typ, local_name, _) =
      let new_local = L.build_alloca (ltype_of_typ typ) local_name builder
      in StringMap.add local_name new_local map
    in

    let local_vars =
      (* create formal vars first *)
      let func_formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
      (Array.to_list (L.params the_function)) in
      (* add local vars to formal vars for complete list *)
      List.fold_left add_local func_formals fdecl.slocals
    in

    (************** Formatting expressions, for printf-ing ********************)
    let format_str fmt = L.build_global_stringptr fmt "fmt" builder in

    (************ Construct code for an expression; return its value **********)
    let rec expr builder ((_, e) : sexpr) =
      match e with
        SILiteral i        -> L.const_int i32_t i
      | SBoolLit b         -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral l        -> L.const_float_of_string float_t l
      | SStringLit s       -> L.build_global_stringptr s "str" builder
      | SId var            -> L.build_load (lookup_variable var local_vars) var builder
      | SAssign (var, e)   -> let e' = expr builder e in
          ignore(L.build_store e' (lookup_variable var local_vars) builder); e'

      (* Binary operation between two integers *)
      | SBinop(((A.Int, _) as e1), op, ((A.Int, _) as e2)) ->
          let lhs = expr builder e1
          and rhs = expr builder e2 in
            (match op with
                A.Add     -> L.build_add
              | A.Sub     -> L.build_sub
              | A.Mult    -> L.build_mul
              | A.Div     -> L.build_sdiv
              | A.Equal   -> L.build_icmp L.Icmp.Eq
              | A.Neq     -> L.build_icmp L.Icmp.Ne
              | A.Less    -> L.build_icmp L.Icmp.Slt
              | A.Greater -> L.build_icmp L.Icmp.Sgt
              | _         -> raise (NotYetSupported("found binary operation not supported for two integers"))
            ) lhs rhs "binop_int_tmp" builder

      (* Binary operation between one or more floats *)
      | SBinop(((A.Float, _) as e1), op, ((A.Int, _) as e2))
      | SBinop(((A.Int, _) as e1), op, ((A.Float, _) as e2))
      | SBinop(((A.Float, _) as e1), op, ((A.Float, _) as e2)) ->
        let lhs = expr builder e1
        and rhs = expr builder e2 in
          (match op with
              A.Add     -> L.build_fadd
            | A.Sub     -> L.build_fsub
            | A.Mult    -> L.build_fmul
            | A.Div     -> L.build_fdiv
            | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
            | A.Neq     -> L.build_fcmp L.Fcmp.One
            | A.Less    -> L.build_fcmp L.Fcmp.Olt
            | A.Greater -> L.build_fcmp L.Fcmp.Ogt
            | _         -> raise (NotYetSupported("found binary operation not supported for one or more floats"))
          ) lhs rhs "binop_float_tmp" builder

      (* Binary operation for two booleans *)
      | SBinop(((A.Bool, _) as e1), op, ((A.Bool, _) as e2)) ->
        let lhs = expr builder e1
        and rhs = expr builder e2 in
          (match op with
              A.Or      -> L.build_or
            | A.And     -> L.build_and
            | _         -> raise (NotYetSupported("found binary operation not supported for two boolean values"))
          ) lhs rhs "binop_bool_tmp" builder

      (* Call to built in printf function *)
      | SFunctionCall ("Meow", [arg]) ->
	        L.build_call printf_func [| format_str (format arg) ; (expr builder arg) |] "printf" builder

      (* Call a user-defined function *)
      | SFunctionCall (fname, args) ->
          let (fdef, fdecl) = lookup_function fname function_decls in
          let llargs = List.rev (List.map (expr builder) (List.rev args))
          and result = (
            match fdecl.styp with
                A.Void -> ""
              | _ -> fname ^ "_result"
          ) in
          L.build_call fdef (Array.of_list llargs) result builder

      | _ -> raise (NotYetSupported("found expr or functions not yet supported"))
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control. This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec stmt builder = function
	      SBlock sl   -> List.fold_left stmt builder sl
      | SExpr e     -> ignore(expr builder e); builder
      | SReturn e   -> ignore(
          match fdecl.styp with
              (* Special "return nothing" instr *)
              A.Void -> L.build_ret_void builder
              (* Build return statement *)
            | _ -> L.build_ret (expr builder e) builder ); builder
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
  in

  List.iter build_function functions;
  the_module
