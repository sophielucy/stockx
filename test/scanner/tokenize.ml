open Parser

let stringify = function
  | LPAREN -> "LPAREN"  | RPAREN -> "RPAREN"
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
