(*
  Performs import-specific semantic checks on the AST,
  Scans and parses each import file,
  Produces AST which is the importing file's AST appended by
  the ASTs of the import files appended
*)
open Exceptions

let imported_asts:(string, Ast.program) Hashtbl.t = Hashtbl.create 10

(* Check that the import exists; note that this only checks for files in the cwd *)
let check_import_exists import =
  if (Sys.file_exists import) then ()
  else raise (ImportNotFound("Could not find import named: " ^ import))

(* Check that import name is capitalized filename without the extension *)
(* I.e. to import hello_world.meow: GIMME HELLO_WORLD? *)
let valid_import_name import =
  let accepted_regex = Str.regexp "['A-Z', '_']+" in
  if Str.string_match accepted_regex import 0 then true else false

(* Convert string representing module into the module path *)
let mangle_import_name import =
  if valid_import_name import then
    let cwd = Sys.getcwd () in
    Printf.sprintf "%s/%s.meow" cwd (String.lowercase_ascii import)
  else
    let msg = "illegal import name " ^ import
    in raise (ImportNotFound (msg))

let import_ast import_path =
  let channel_in = open_in import_path in
  let lexbuf = Lexing.from_channel channel_in in
  let ast = Parser.program Scanner.token lexbuf in
  Hashtbl.add imported_asts import_path ast; ast

(* Pull in the import; this reads in the file, parses into AST
   and adds the file to the *)
let rec do_import import =
  (* get the import as a path*)
  let real_import_path = mangle_import_name import in

  (* if we've already pulled in the import, stop recursion here *)
  if Hashtbl.mem imported_asts real_import_path then ()
  else
    (* if it exists, get the ast; then recursively get other ASTs *)
    check_import_exists real_import_path;
    let new_ast = import_ast real_import_path in
    match new_ast with
      ([], _, _) -> ()
    | (i :: [], _, _) -> do_import i
    | (t, _, _) -> List.iter (fun i -> do_import i) t

(* Add import contents to functions and classes *)
let add_imports (imports, functions, classes) =
  (* populate hash table of asts that we need to import *)
  List.iter do_import imports;

  (* add all the new classes and functions to the list of existing ones *)
  let functions' =
    Hashtbl.fold (fun _ (_, funcs, _) acc ->
      List.fold_left (fun acc func -> func :: acc) acc funcs
  ) imported_asts functions
  and classes' =
    Hashtbl.fold (fun _ (_, _, cls_list) acc ->
      List.fold_left (fun acc cls -> cls :: acc) acc cls_list
  ) imported_asts classes
  in
  ([], functions', classes')
