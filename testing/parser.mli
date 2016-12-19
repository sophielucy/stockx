type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | RETURN
  | RETURNS
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | FLOAT
  | BOOL
  | VOID
  | STRING
  | LITERAL of (int)
  | FLOATLIT of (float)
  | STRINGLIT of (string)
  | ID of (string)
  | FUNCTION
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
