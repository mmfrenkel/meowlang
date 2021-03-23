(*
  Performs import-specific semantic checks on the AST, producing a new SAST
*)
open Exceptions
open Ast

(* Check that the file exists in an expected location *)
let find_imports imports =
  let find_import i =
    match Sys.file_exists i (* this is just the working directory *)
                          (* need to try also path according to LRM, or just change LRM *)
    with
    true -> ()
  | false -> raise (ImportNotFound (undeclared_msg ^ i))
  in
    List.iter find_import imports

(* Check that import name is capitalized filename without the extension *)
(* I.e. to import hello_world.meow: GIMME HELLO_WORLD? *)
let check_import_names imports = 
  let accepted_regex = Str.regexp "(?:_[A-Z]+)*" in
    let check_name i =
      match Str.string_match accepted_regex i 0 with
        true -> ()
      | false -> raise (ImportNotFound ( "illegal import name, example of expected: hello_world.meow imported as GIMME HELLO_WORLD?"))
    in
      List.iter check_name imports

(* Scan each import file *)
let scan_imports imports = imports

(* Parse each scenned import *)
let generate_asts scanned_imports = scanned_imports

(* Add functions from imports to the list of functions *)
let add_import_functions existing_funcs imports =
  let add_import_funcs existing_funcs import = List.map (fun func_name -> func_name :: existing_funcs ) import in
    List.map add_import_funcs imports

(* Add classes from imports to the list of classes *)
let add_import_classes existing_classes imports =
  let add_import_classes existing_classes import = List.map (fun func_name -> func_name :: existing_classes ) import in
    List.map add_import_classes imports

let add (imports, functions, classes) =
  (* check that import file in expected location *)
  find_imports imports;
  (* check that the import file name is legal *)
  check_import_names imports;

  (* scan each file: lexbuf = Lexing.from_channel each_import *)
  let scanned_imports = scan_imports imports in
    (* parse each file: import_ast = Parser.program Scanner.token lexbuf *)
    let import_asts = generate_asts scanned_imports in 
      (* add functions from each import_ast to functions except for import_ast main function *)
      let functions' = add_import_functions functions import_asts in
      (* add classes from each import_ast to classes *)
      let classes' = add_import_classes classes import_asts in 
  
  (functions', classes')
