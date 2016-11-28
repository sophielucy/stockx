(* Ocamllex scanner for StockX *)

{
    open Parser
    let lineno = ref 1
}

(* need to exclude newline for single line comments *)
let whitespace = [' ' '\t' '\r' '\n']
let alpha = ['a'-'z' 'A'-'Z']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let int = digit+
let float = (digit*) ['.'] digit+
let char = ''' ( ascii | digit ) '''
let string = '"' (ascii)* '"'
let id = alpha (alpha | digit | '_')*

rule token = parse
  whitespace    { token lexbuf } (* white space *)
| "/*"          { comment lexbuf }
| "//"          { line_comment lexbuf }

| '('           { LPAREN }
| ')'           { RPAREN }
| '{'           { LBRACE }
| '}'           { RBRACE }
| ';'           { SEMI }
| ','           { COMMA }
| "function"    { FUNCTION }

(* operators *)
| '+'       { PLUS }
| '-'       { MINUS }
| '*'       { TIMES }
| '/'       { DIVIDE }
| '%'       { MODULO }
| "+="      { PLUSEQ }
| "-="      { MINUSEQ }
| "*="      { TIMESEQ }
| "/="      { DIVIDEEQ }
| "%="      { MODULOEQ }
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
| "void"    { VOID }
| "null"    { NULL }
| "true"    { TRUE }
| "false"   { FALSE }
| "stock"   { STOCK }
| "order"   { ORDER }
| "portfolio"   { PORTFOLIO }
| "struct"  { STRUCT }
| "array"   { ARRAY }
| "string"  { STRING }

| int as lxm        { INT_LITERAL(int_of_string lxm) }
| float as lxm      { FLOAT_LITERAL(float_of_string lxm) }
| id as lxm         { ID(lxm) }
| string as lxm     { STRING_LITERAL(lxm) }
| eof               { EOF }
| _ as char         { raise (Failure("illegal character " ^ Char.escaped char)) }
(*
| _ as illegal  { raise (Exceptions.IllegalCharacter(illegal, !lineno)) }
*)

and comment = parse
    "*/"        { token lexbuf }
|   _           { comment lexbuf }

and line_comment = parse
    ['\n' '\r'] { token lexbuf }
|   _           { line_comment lexbuf }