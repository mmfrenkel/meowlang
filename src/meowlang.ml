(*
    Mostly a place holder file if we wanted to test our scanner/parser
*)
open Ast

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Scanner.token lexbuf in
    print_string (string_of_program ast)
