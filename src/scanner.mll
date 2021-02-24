(* Ocamllex scanner for MicroC

Support for strings through read_string taken from OCaml documentation
found https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html
*)
{
    open Parser

    exception SyntaxError of string
}

let digit = ['0' - '9']
let digits = digit+
let identifier = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let whitespace = [' ' '\t' '\r' '\n']

(*
  Special rule for float; note that because periods are used as a semicolon
  in this langauge, we have to treat the input "2." differently than usual. In
  this language, "2." is interpreted as an integer and "2.0" is a float.
*)
let float = digits '.'  ((digit+ | ( ['e' 'E'] ['+' '-']? digits)) | (digit* ( ['e' 'E'] ['+' '-']? digits )))

rule token = parse
(* Whitespace/Comments *)
  whitespace      { token lexbuf }
| "PSST"          { comment lexbuf }

(* Structural *)
| "HAI"           { LBRACE }
| "KBYE"          { RBRACE }
| "WIT"           { LPAREN }
| ","             { RPAREN }
| "AN"            { COMMA }
| "."             { SEMI }

(* Module/Imports *)
| "GIMME"         { MODULE }
| "?"             { IMPORT }

(* Objects/Classes *)
| "MAEK"          { MAKE }
| "NEW"           { NEW }
| "BLEEP"         { FREE }
| "CLASS"         { CLASS }
| "IN"            { IN }

(* Functions *)
| "PURR"          { CALL }
| "FUNC"          { FUNCTION }
| "GIVE"          { RETURN }

(* Operators *)
| "SUM OF"        { PLUS }
| "DIFF OF"       { MINUS }
| "PRODUKT OF"    { TIMES }
| "QUOSHUNT OF"   { DIVIDE }
| "ITZ ME"        { DEF }
| "IZ"            { ASSIGN }
| "SAEM"          { EQ }
| "DIFFRINT"      { NEQ }
| "SMALLR"        { LT }
| "BIGGR"         { GT }
| "BOTH OF"       { AND }
| "EITHER OF"     { OR }
| "NOT"           { NOT }
| "CAT"           { CONCAT }
| "THAN"          { COMP }
| "OF"            { CONTAINS }
| "UPPIN"         { INCREMENT }
| "NERFIN"        { DECREMENT }

(* Flow Control *)
| "O RLY?"               { IF       }
| "YA RLY"               { THEN     }
| "NO WAI"               { ELSE     }
| "IM IN YR LOOP"        { FOR      }

(* Data Types *)
| "YARN"          { STRING }
| "NUMBR"         { INT }
| "BOO"           { BOOL }
| "NUMBAR"        { FLOAT }
| "AYE"           { BLIT(true)  }
| "NAY"           { BLIT(false) }
| "BUCKET"        { ARRAY }
| digits as lxm   { ILIT(int_of_string lxm) }
| float as lxm    { FLIT(lxm) }
| identifier as lxm { ID(lxm) }
| '"'             { read_string (Buffer.create 17) lexbuf }  (* String *)
| eof { EOF }
| _ as char { raise (SyntaxError("Illegal character " ^ Char.escaped char)) }

and read_string buf =
  parse
  | '"'           { SLIT(Buffer.contents buf) }
  | [^ '"']+
    {
        Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
    }
  | eof { raise (SyntaxError ("String is not terminated")) }

and comment = parse
  "\n" { token lexbuf }
| _    { comment lexbuf }
