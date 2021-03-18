(*
    Main CLI for running the compiler. Command line arguments specify behavior.
*)
open Ast
open Pretty

type action = Ast | Sast

let _ =
    (* Figure out the action to take *)
    let action = ref Ast in
    let set_action a () = action := a in
    let speclist = [
        ("-a", Arg.Unit (set_action Ast), "Print the AST");
        ("-s", Arg.Unit (set_action Sast), "Check Semantics");
    ] in
    let usage_msg = "usage: ./meowlang.native [-a|-s] [file.meow]" in
    let channel = ref stdin in
    Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

    (* Perform action based on specification*)
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Scanner.token lexbuf in

    match !action with
        Ast -> print_string (string_of_program ast)
      | Sast -> let sast = Semant.check ast in
        match sast with
              ([],  func_decl, []) -> print_string "Semantic check (functions only) succeeded!\n"
            | _ -> print_string "Semantic check failed!\n"
