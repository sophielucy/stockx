(* Ocamllex scanner for MATHLANG *)

{ 
  open Parser 
  let lineno = ref 1
}

let whitespace = [' ' '\t' '\r' '\n']
let alpha = ['a'-'z' 'A'-'Z']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let int = digit+
let float = (digit*) ['.'] digit+
let string = '"' ((ascii)* as s) '"'
let id = alpha (alpha | digit | '_')*

rule token = parse
  whitespace    { token lexbuf } (* white space *)
| "/*"          { comment lexbuf }
| "//"          { line_comment lexbuf }
| '('           { LPAREN }
| ')'           { RPAREN }
| '{'           { LBRACE }
| '}'           { RBRACE }
| '['           { LBRACKET }
| ']'           { RBRACKET }
| ';'           { SEMI }
| ','           { COMMA }
| "function"    { FUNCTION }

(* operators *)
| '+'       { PLUS }
| '-'       { MINUS }
| '*'       { TIMES }
| '/'       { DIVIDE }
| "+="      { PLUSEQ }
| "-="      { MINUSEQ }
| "*="      { TIMESEQ }
| "/="      { DIVIDEEQ }
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
| '|'       { BAR }

(* branch control *)
| "if"      { IF }
| "else"    { ELSE }
| "for"     { FOR }
| "while"   { WHILE }
| "return"  { RETURN }
| "returns" { RETURNS }

(* Data Types *)
| "int"     { INT }
| "float"   { FLOAT }
| "bool"    { BOOL }
| "void"    { VOID }
| "true"    { TRUE }
| "false"   { FALSE }
| "array"   { ARRAY }
| "string"  { STRING }

(* Literals *)
| int as lxm        { INT_LITERAL(int_of_string lxm) }
| float as lxm      { FLOAT_LITERAL(float_of_string lxm) }
| id as lxm         { ID(lxm) }
| string            { STRING_LITERAL(s) }
| eof               { EOF }
| _ as char         { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and line_comment = parse
    ['\n' '\r'] { token lexbuf }
|   _           { line_comment lexbuf }
