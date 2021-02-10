(*
    REPLACE WITH CODE RELEVANT TO OUR PROJECT
*)

{ open Parser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| eof { EOF }
