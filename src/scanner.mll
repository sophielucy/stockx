(* Ocamllex scanner for StockX *)

{ open Parser }

(* need to exclude newline for single line comments *)
let whitespace = [' ' '\t' '\r' '\n']
let alpha = ['a'-'z' 'A'-'Z']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let int = digit+
let float = (digit+) ['.'] digit+
let char = ''' ( ascii | digit ) '''
let id = alpha (alpha | digit | '_')*

rule token = parse
  whitespace    { token lexbuf } (* white space *)
| "/*"      { comment lexbuf }

| '('       { LPAREN }
| ')'       { RPAREN }
| '{'       { LBRACE }
| '}'       { RBRACE }
| ';'       { SEMI }
| ','       { COMMA }

(* operators *)
| '+'       { PLUS }
| '-'       { MINUS }
| '*'       { TIMES }
| '/'       { DIVIDE }
| '%'       { MODULO }
| '='       { ASSIGN }
| "=="      { EQ }
| "!="      { NEQ }
| '<'       { LT }
| "<="      { LEQ }
| '>'       { GT }
| ">="      { GEQ }
| "and"     { AND }
| "or"      { OR }
| "not"     { NOT }
| '.'       { DOT }
| '['       { LBRACKET }
| ']'       { RBRACKET }
| '|'       { BAR }

(* branch control *)
| "if"      { IF }
| "else"    { ELSE }
| "for"     { FOR }
| "while"   { WHILE }
| "return"  { RETURN }

(* Data Types *)
| "int"     { INT }
| "float"   { FLOAT }
| "bool"    { BOOL }
| "char"    { CHAR }
| "void"    { VOID }
| "null"    { NULL }
| "true"    { TRUE }
| "false"   { FALSE }

| int as lxm        { INT_LITERAL(int_of_string lxm) }
| float as lxm      { FLOAT_LITERAL(float_of_string lxm) }
| char as lxm       { CHAR_LITERAL( String.get lxm 1 ) }
| id as lxm         { ID(lxm) }
| eof               { EOF }

and comment = parse
    "*/"    { token lexbuf }
|   _       { comment lexbuf }
