(*
  Performs import-specific semantic checks on the AST,
  Scans and parses each import file,
  Produces AST which is the importing file's AST appended by
  the ASTs of the import files appended
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

(* Scan each import file, returns a list of lexbuf *)
(* imports: list of filenames *)
let scan_imports imports =
    (* change this to open, scan, close in one List.map step *)
    let channel_in fname = open_in fname in
      let scan channel = Lexing.from_channel channel in
        List.map scan (List.map channel_in imports)
        
(* Parse each scenned import, returns a list of AST programs *)
(* lexbuf_imports: list of Lexing.lexbuf *)
let generate_asts lexbuf_imports =
  let parse_ast lexbuf = Parser.program Scanner.token lexbuf in
    List.map parse_ast lexbuf_imports

(* Add functions from imports to the list of functions *)
(* existing_funcs: list of funcs *)
(* import_asts: import list * func_decl list * class_decl list *)
let add_import_functions existing_funcs import_asts =
  let add_import_funcs existing_funcs import = List.map (fun (imports, func_decl, class_decl) -> func_decl :: existing_funcs ) import in
    List.map add_import_funcs import_asts

(* Add classes from imports to the list of classes *)
(* existing_classes: list of classes *)
(* import_asts: import list * func_decl list * class_decl list *)
let add_import_classes existing_classes import_asts =
  let add_import_classes existing_classes import = List.map (fun (imports, func_decl, class_decl) -> class_decl :: existing_classes ) import in
    List.map add_import_classes import_asts

(* imports: list of filename imports from importing program *)
(* functions: list of func_decls *)
(* classes: list of class_decls *)
let add (imports, functions, classes) =
  (* check that import file in expected location *)
  find_imports imports;
  (* check that the import file name is legal *)
  check_import_names imports;

  (* scan each file: lexbuf = Lexing.from_channel each_import *)
  let lexbuf_imports = scan_imports imports in
    (* parse each file: import_ast = Parser.program Scanner.token lexbuf *)
    let import_asts = generate_asts lexbuf_imports in 
      (* add functions from each import_ast to functions except for import_ast main function *)
      let functions' = add_import_functions functions import_asts in
      (* add classes from each import_ast to classes *)
      let classes' = add_import_classes classes import_asts in 

      (* something like pattern matching to check if imports have imports? *)
  
  (functions', classes')