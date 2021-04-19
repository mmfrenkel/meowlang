(*
    Main CLI for running the compiler. Command line arguments specify behavior.
*)
open Pretty

type action = Ast | Sast | LLVM_IR | Compile

let _ =
    (* Figure out the action to take *)
    let action = ref Ast in
    let set_action a () = action := a in
    let speclist = [
        ("-a", Arg.Unit (set_action Ast), "Print the AST");
        ("-s", Arg.Unit (set_action Sast), "Perform Semantic Checks on the SAST");
        ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
        ("-c", Arg.Unit (set_action Compile), "Check and print the generated LLVM IR");
    ] in
    let usage_msg = "usage: ./meowlang.native [-a|-s|-l|-c] [file.meow]" in
    let channel = ref stdin in
    Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

    (* Perform action based on specification*)
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Scanner.token lexbuf in
    let ast_with_imports = Import.add_imports ast in

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
