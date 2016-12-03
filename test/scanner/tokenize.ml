open Parser

let stringify = function
  | LPAREN  -> "LPAREN" | RPAREN    -> "RPAREN"
  | LBRACE  -> "LBRACE" | RBRACE    -> "RBRACE"
  | SEMI    -> "SEMI"   | COMMA     -> "COMMA"

  | PLUS    -> "PLUS"   | MINUS     -> "MINUS"
  | TIMES   -> "TIMES"  | DIVIDE    -> "DIVIDE"
  | MODULO  -> "MODULO"

  | PLUSEQ  -> "PLUSEQ"     | MINUSEQ   -> "MINUSEQ"
  | TIMESEQ -> "TIMESEQ"    | DIVIDEEQ  -> "DIVIDEEQ"
  | MODULOEQ -> "MODULOEQ"

  | ASSIGN  -> "ASSIGN"

  | EQ      -> "EQ"     | NEQ       -> "NEQ"
  | LT      -> "LT"     | LEQ       -> "LEQ"
  | GT      -> "GT"     | GEQ       -> "GEQ"
  | AND     -> "AND"    | OR        -> "OR"
  | NOT     -> "NOT"






  | FLOAT_LITERAL(float) -> "FLOAT_LITERAL"
  | _ -> "hurrdurr"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)
