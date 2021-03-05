open Ast

module StringMap = Map.Make(String)

let check_func_duplicates map fd =
  match fd with (* No duplicate functions allowed *)
     |  _ when StringMap.mem fd.fname map ->
        raise (Failure ("Function already defined: " ^ fd.fname))
     | _ ->  StringMap.add fd.fname fd map

let check_cls_duplicates map cls =
  match cls with (* No duplicate functions allowed *)
    |  _ when StringMap.mem cls.cname map ->
      raise (Failure ("Class already defined: " ^ cls.cname))
    | _ ->  StringMap.add cls.cname cls map

let check_method_duplicates map cls =
  let _ = List.fold_left check_func_duplicates StringMap.empty cls.cfuncs
  in StringMap.add cls.cname cls.cfuncs map

let func_exists s map =
  if StringMap.mem s map then true else raise (Failure ("Function or method not found: " ^ s))

let check (imports, functions, classes) =

  (* Check for duplicate function, method and class names *)
  let function_decls = List.fold_left check_func_duplicates StringMap.empty functions
  and _ = List.fold_left check_cls_duplicates StringMap.empty classes
  and _ = List.fold_left check_method_duplicates StringMap.empty classes
  in
  func_exists "Main" function_decls
