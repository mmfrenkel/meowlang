(*Authors:
Carolyn Chen
Lauren Pham
*)
(*
  Performs import-specific semantic checks on the AST,
  Scans and parses each import file,
  Produces AST which is the importing file's AST appended by
  the ASTs of the import files appended
*)
open Exceptions
open Ast

(*Convert import names to string*)
(*Also adds file extension ".meow" for testing purposes*)
let convert_str imports =
  let convert_s i =
  match i
  with
  Module(n) -> n ^ ".meow" in 
  List.map convert_s imports

(* Check that the file exists in an expected location *)
let find_imports imports =
  let find_import i =
    match Sys.file_exists i with 
    true -> ()
  | false -> raise (ImportNotFound (undeclared_msg ^ i))
  in List.iter find_import imports

(* Check for duplicate imports from visited imports*)
(* imports: list of filenames *)
let visited = [] 
let check_dup_imports imports =
  let check_visited import_name =
    match List.mem import_name visited with 
     true -> raise (DuplicateImport (dup_import_msg ^ import_name))
   | false -> ()
  in
  List.iter check_visited imports

(* Check that import name is capitalized filename without the extension *)
(* I.e. to import hello_world.meow: GIMME HELLO_WORLD? *)

let check_import_names imports = 
  let accepted_regex = Str.regexp "[A-Z]+[_A-Z]*" in
    let check_name i =
      match Str.string_match accepted_regex i 0 with
        (*if true, add to list of visited imports*)
        true -> i :: visited
      | false -> raise (ImportNotFound ( "Illegal import name, example of expected: hello_world.meow imported as GIMME HELLO_WORLD?"))
    in
    List.map check_name imports


(* Scan each import file, returns a list of lexbuf (lexbuf_imports) *)
(* imports: list of filenames *)
(* scan each import file from list of imports in the current file*)
let scan_imports imports =
    (* change this to open, scan, close in one List.map step *)
    let channel_in fname = open_in fname in
      let scan channel = Lexing.from_channel channel in
        List.map scan (List.map channel_in imports)
      
(* Parse each scanned import, returns a list of AST programs (import_asts) *)
(* lexbuf_imports: list of Lexing.lexbuf *)
let generate_asts lexbuf_imports =
  let parse_ast lexbuf = Parser.program Scanner.token lexbuf in
    List.map parse_ast lexbuf_imports

(***********************************************************************)
(* imports: list of filename imports from importing program *)
(* functions: list of func_decls *)
(* classes: list of class_decls *)
let add ((imports : import list), (functions : func_decl list), (classes : class_decl list)) =
  (*Import Checks*)
  let str_imports = convert_str imports in 
  (* Check that import file in expected location *)
  let check_loc = find_imports str_imports in 
  (* Check for duplicate imports from visited imports*)
  let check_dup = check_dup_imports str_imports in
  (* Check that the import file name is legal *)
  let check_iname = check_import_names str_imports in
  (* scan each file: lexbuf = Lexing.from_channel each_import *)
  let lexbuf_imports = scan_imports str_imports in
  (* parse each file: import_ast = Parser.program Scanner.token lexbuf *)
  let import_asts = generate_asts lexbuf_imports  

(***********************************************************************)
in
(*Flatten ASTs, get new func_decl list and class_decl list to pass to Semant*)
let get_fdecls imports = 
  let get_fd t =
    match t with
    ((a : import list), (b : func_decl list), (c : class_decl list)) -> b
    in
get_fd imports
in
  let get_cdecls imports = 
      let get_cd t =
        match t with
        ((a : import list), (b : func_decl list), (c : class_decl list)) -> c
        in
get_cd imports

in
(*Source: David Farthing @ https://stackoverflow.com/questions/37891315/ocaml-make-a-list-one-level-more-shallow-the-opposite-of-list-singleton*)
let rec fshallow (lst:func_decl list list) : (func_decl list) = 
  match lst with
  | [] -> []
  | h::t -> h@(fshallow t)

in
(*TO DO: Remove Main functions, cannot be duplicate*)

let rec cshallow (lst:class_decl list list) : (class_decl list) = 
  match lst with
  | [] -> []
  | h::t -> h@(cshallow t)
 
in
      let if_list = List.map get_fdecls import_asts in
      let ic_list = List.map get_cdecls import_asts in

      let functions = functions :: if_list in
      let classes = classes :: ic_list in

      let functions = fshallow functions in
      let classes = cshallow classes in
 
  ([], functions, classes)