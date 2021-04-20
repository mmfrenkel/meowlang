(*
    Main CLI for running the compiler. Command line arguments specify behavior.
*)
open Pretty
type action = Ast | Sast | LLVM_IR | Compile
let filename = ref ""
let _ =
    (* Figure out the action to take *)
    let action = ref Ast in
    let set_action a () = action := a in
    let set_filename name = filename := name in
    let speclist = [
        ("-a", Arg.Unit (set_action Ast), "Print the AST");
        ("-s", Arg.Unit (set_action Sast), "Perform Semantic Checks on the SAST");
        ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
        ("-c", Arg.Unit (set_action Compile), "Check and print the generated LLVM IR");
        ("-f", Arg.String (set_filename), "Submit file to compile")
    ] in
    let usage_msg = "usage: ./meowlang.native [-a|-s|-l|-c] -f [file.meow]" in
    Arg.parse speclist print_endline usage_msg;

    (* Open the specified file *)
    let channel_in = open_in !filename in
    let lexbuf = Lexing.from_channel channel_in in
    let ast = Parser.program Scanner.token lexbuf in
    let ast_with_imports = Import.add_imports ast !filename in

    (* Perform action based on specification *)
    match !action with
      Ast -> print_string (string_of_program ast_with_imports) 
    | _   ->
        let sast = Semant.check ast_with_imports in
        match !action with
          Ast     -> ()
        | Sast    -> print_string "Semantic check succeded!\n"
        | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate sast))
        | Compile -> let m = Codegen.translate sast in
            Llvm_analysis.assert_valid_module m;
            print_string (Llvm.string_of_llmodule m)