
{
open Parser

exception LexerError of string

  let escaped_chars = [
    ('\\', "\\");
    ('\'', "'");
    ('"', "\"");
    ('n', "\n");
    ('t', "\t");
    ('b', "\b");
    ('r', "\r");
  ]

  let escaped_conv char =
    try 
      List.assoc char escaped_chars
    with Not_found ->
      failwith "escaped_conv error"

let stoi s =
  if String.length s >= 2 && s.[0] = '0' && s.[1] < '8' then
    int_of_string ("0o" ^ s)
  else
    int_of_string s

}

let lid = ['a'-'z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

let uid = ['A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9' ]*

let int = ['0'-'9'] ['0'-'9' '_']*

let int_ =
    ( ("0x" | "0X") ['0'-'9' 'a'-'f' 'A'-'F'] (['0'-'9' 'a'-'f' 'A'-'F'] | '_')*
    | ("0o" | "0O") ['0'-'7'] ['0'-'7' '_']*
    | ("0b" | "0B") ['0' '1'] ['0' '1' '_']*)

let float =
  '-'? ['0'-'9'] ['0'-'9' '_']*
  (('.' ['0'-'9' '_']*) (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)? |
   ('.' ['0'-'9' '_']*)? (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*))


rule token = parse
| [' ' '\t' '\r']
  { token lexbuf }
| '\n'
  { Lexing.new_line lexbuf; token lexbuf }
| ';'
  { SEMICOLON }
| ','
  { COMMA }
| "struct"
  { STRUCT }
| "enum"
  { ENUM }
| "const"
  { token lexbuf }
| "if"
  { IF }
| "else"
  { ELSE }
| "while"
  { WHILE }
| "for"
  { FOR }
| "in"
  { IN }
| "return"
  { RETURN }
| "continue"
  { CONTINUE }
| "break"
  { BREAK }
| "switch"
  { SWITCH }
| "case"
  { CASE }
| '+'
  { PLUS }
| "++"
  { INC }
| '-'
  { MINUS }
| '!'
  { BANG }
| ':'
  { COLON }
| "--"
  { DEC }
| '*'
  { STAR }
| '/'
  { DIV }
| '%'
  { MOD }
| "<<"
  { LSHIFT }
| ">>"
  { RSHIFT }
| '.'
  { DOT }
| "->"
  { ARROW }
| '&'
  { AND }
| '^'
  { HAT }
| '|'
  { OR }
| "&&"
  { ANDAND }
| "||"
  { OROR }
| '~'
  { NOT }
| "=="
  { EQEQ }
| "!="
  { NE }
| "="
  { EQ }
(*| "+="
  { ADD_EQ }
| "-="
  { SUB_EQ }
| "*="
  { MUL_EQ }
| "/="
  { DIV_EQ }
| "%="
  { MOD_EQ }
| "<<="
  { LSHIFT_EQ }
| ">>="
  { RSHIFT_EQ }
| "&="
  { AND_EQ }
| "^="
  { XOR_EQ }
| "|="
  { OR_EQ } *)
| "<"
  { LT }
| ">"
  { GT }
| "<="
  { LE }
| ">="
  { GE }
| '('
  { LPAREN }
| ')'
  { RPAREN }
| '{'
  { LBRACE }
| '}'
  { RBRACE }
| '['
  { LBRACKET }
| ']'
  { RBRACKET }
| "sizeof"
  { SIZEOF }
| "//"
  { commentbis lexbuf }
| "/*"
  { comment lexbuf }
| "true"              
  { BOOL true }
| "false"             
  { BOOL false }
| int                 
  { INT (int_of_string (Lexing.lexeme lexbuf)) }
| int_                
  { INT (int_of_string (Lexing.lexeme lexbuf)) }
| "'" ([^ '\\' '\''] as c) "'"  
  { CHAR c }
| "'" '\\' (['\\' '\'' 'n' 't' 'b' 'r'] as c) "'" 
  { CHAR ((escaped_conv c).[0]) }
| (float as f)
  { FLOAT (float_of_string f) }
| '"'                 
  { STRING (string "" lexbuf) }
| lid as l
  {
    LID l
  }
| uid as u
  {
    UID u
  }
| eof
  { EOF }
| _
  { raise (LexerError ("illegal token '%s'" ^ Lexing.lexeme lexbuf)) }


and string acc = parse
| '"'   { acc }
| '\\' (['\\' '"' 'n' 't' 'b' 'r'] as c) { string (acc ^ (escaped_conv c)) lexbuf }
| eof { failwith "lexer token error" }
| _ { string (acc ^ (Lexing.lexeme lexbuf)) lexbuf }

and comment = parse
| "*/"
  { token lexbuf }
| '\n'
  { Lexing.new_line lexbuf; comment lexbuf }
| eof
  { raise (LexerError "unterminated comment") }
| _
  { comment lexbuf }

and commentbis = parse
| '\n'
  { Lexing.new_line lexbuf; token lexbuf }
| eof
  { EOF }
| _
  { commentbis lexbuf }