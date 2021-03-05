(*
    Mostly a place holder file if we wanted to test our scanner/parser
*)
open Ast
open Pretty

type action = Abs_Syntax_Tree | Semantics

let _ =
    (* Figure out the action to take *)
    let action = ref Abs_Syntax_Tree in
    let set_action a () = action := a in
    let speclist = [
        ("-a", Arg.Unit (set_action Abs_Syntax_Tree), "Print the AST");
        ("-s", Arg.Unit (set_action Semantics), "Check Semantics");
    ] in
    let usage_msg = "usage: ./meowlang.native [-a|-s] [file.meow]" in
    let channel = ref stdin in
    Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

    (* Perform action based on specification*)
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Scanner.token lexbuf in

    match !action with
        Abs_Syntax_Tree -> print_string (string_of_program ast);
      | Semantics -> let result = Semant.check ast in
        if result then print_string "Semantic check succeeded!\n"
