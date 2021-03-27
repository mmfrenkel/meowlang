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
let lookup_variable var_name =
  try Hashtbl.find local_variables var_name
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
let i32_t     = L.i32_type    context
let i1_t      = L.i1_type     context
let float_t   = L.double_type context
let void_t    = L.void_type   context
let str_t     = L.pointer_type  (L.i8_type  context)

(* Finds the LLVM type corresponding to the Meowlang type *)
let ltype_of_typ = function
    A.Int       -> i32_t
  | A.Bool      -> i1_t
  | A.Float     -> float_t
  | A.Void      -> void_t
  | A.String    -> str_t
  | A.Obtype(c) -> L.pointer_type (find_struct_by_cls c)
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
  | _ -> raise (NotYetSupported("formatting for complex expr and functions not yet supported"))


(*********************************************************)
(* Build Function: Fill in the body of the each function *)
(*********************************************************)
let build_function fdecl =

  (* Create the prototype for printf/Meow, a built-in function
     TODO: figure out how to not do this for each function *)
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| str_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  let (the_function, _) = Hashtbl.find global_functions fdecl.sfname in
  let builder = L.builder_at_end context (L.entry_block the_function) in

  (********** Construct the functions locals (formal + local vars) ************)
  let add_formal acc (typ, formal_name) param =
      (* set name of param value to corresponding formal name *)
      L.set_value_name formal_name param;

      (* allocate space stack for formal *)
      let new_formal = L.build_alloca (ltype_of_typ typ) formal_name builder in
      ignore (L.build_store param new_formal builder);  (* store %param %new_formal *)
      Hashtbl.add local_variables formal_name new_formal;
      (formal_name, new_formal) :: acc (* to make compiler happy *)

  and add_local (typ, local_name, _) =
    let new_local = L.build_alloca (ltype_of_typ typ) local_name builder
    in Hashtbl.add local_variables local_name new_local
  in

  (* Clear existing locals and start over *)
  Hashtbl.clear local_variables;
  ignore(List.fold_left2 add_formal [] fdecl.sformals (Array.to_list (L.params the_function)));
  List.iter add_local fdecl.slocals;

  (************** Formatting expressions, for printf-ing ********************)
  let format_str fmt = L.build_global_stringptr fmt "fmt" builder in

  (************ Construct code for an expression; return its value **********)
  let rec expr builder ((_, e) : sexpr) =
    match e with
      SILiteral i        -> L.const_int i32_t i
    | SBoolLit b         -> L.const_int i1_t (if b then 1 else 0)
    | SFliteral l        -> L.const_float_of_string float_t l
    | SStringLit s       -> L.build_global_stringptr s "str" builder
    | SId var            -> L.build_load (lookup_variable var) var builder

    | SAssign ((_, lhs), e)   ->
        let rhs = expr builder e in
        (match lhs with
          (* Used in assigning variables; not used in assigning class members *)
            SId(var) ->
              ignore(L.build_store rhs (lookup_variable var) builder); rhs
          (* Handle this class access PLUS Assignment case -- this could be merged with ClassAssign, if time allows *)
          | SClassAccess(A.Obtype(cname), v, inst_v) ->
              let index = lookup_index cname inst_v
              and load_tmp = expr builder v in
              let lhs = L.build_struct_gep load_tmp index "tmp" builder in
              ignore(L.build_store rhs lhs builder); rhs
          | _ ->
            raise (NotYetSupported("codegen: assignment not supported for anything but class instance vars and regular variables")))

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
        let (fdef, fdecl) = lookup_function fname in
        let llargs = List.rev (List.map (expr builder) (List.rev args))
        and result = (
          match fdecl.styp with
              A.Void -> ""
            | _ -> fname ^ "_result"
        ) in
        L.build_call fdef (Array.of_list llargs) result builder

    (* Create a new struct instance of class c with variable name n*)
    | SNewInstance(v, c, _) ->
        (match c with
         A.Obtype (cname) as cls -> (
          (* add new variable to local vars; a pointer to malloc'd item *)
          add_local (cls, v, None);
          (* assign the malloc to the new variable *)
          let rhs = L.build_malloc (find_struct_by_cls cname) "new_struct" builder
          and lhs = lookup_variable v in
          ignore(L.build_store rhs lhs builder); rhs
         )
        | _ -> raise (ObjectCreationInvalid("codegen: cannot create instance of anything but Obtype")))

    | SClassAccess(A.Obtype(cname), v, inst_v) ->
        let tmp_value = expr builder v
        and index = lookup_index cname inst_v in
        let deref = L.build_struct_gep tmp_value index "tmp" builder in
        L.build_load deref "dr" builder

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
          | _ -> L.build_ret (expr builder e) builder
      ); builder

    | SClassAssign (A.Obtype(cname), v, inst_v, e) ->
        let rhs = expr builder e
        and index = lookup_index cname inst_v
        and load_tmp = expr builder v in
        let lhs = L.build_struct_gep load_tmp index "tmp" builder in
        ignore(L.build_store rhs lhs builder); builder

    | SDealloc (v) ->
        let tmp_value = expr builder v in
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

(****************************************************************
  Translate : Takes a Sast.program and produces an Llvm.module

  Note that all "actionable" items in the program are now functions
  (there are no longer class methods/etc). Classes are useful because
  they provide the struct definition corresponding to the class, but
  all class methods have been lifted to global scope in the semantic
  checking step.
*****************************************************************)
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
