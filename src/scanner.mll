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
let float = digits '.'  ((digit+ | ( ['e' 'E'] ['+' '-']? digits)) | (digit* ( ['e' 'E'] ['+' '-']? digits )))

rule token = parse
  whitespace      { token lexbuf }             (* Whitespace *)
| "PSST"          { comment lexbuf }           (* Comments *)
| "GIMME"         { MODULE }
| "?"             { IMPORT }
| "HAI"           { LBRACE }
| "KBYE"          { RBRACE }
| "PURR"          { CALL }
| "FUNC"          { FUNCTION }
| "WIT"           { LPAREN }
| ","             { RPAREN }
| "AN"            { COMMA }
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
| "YARN"          { STRING }
| "GIVE"          { RETURN }
| "NUMBR"         { INT }
| "BOO"           { BOOL }
| "NUMBAR"        { FLOAT }
| "AYE"           { BLIT(true)  }
| "NAY"           { BLIT(false) }
| "CAT"           { CONCAT }
| "THAN"          { COMP }
| digits as lxm   { ILIT(int_of_string lxm) }
| float as lxm    { FLIT(lxm) }
| identifier as lxm { ID(lxm) }
| '"'             { read_string (Buffer.create 17) lexbuf }
| "."             { SEMI }
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
