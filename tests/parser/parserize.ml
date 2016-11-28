open Ast

let rec txt_of_stmt = function
  | Expr(expr) -> "Fantastic"
  | _ -> "Wow"

let rec txt_of_stmts stor = function
  | [] -> stor
  | stmt :: tl -> txt_of_stmts (txt_of_stmt stmt :: stor) tl

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in

  let helper (func_decls, stmts) = stmts
  in

  let ast = helper ast in

  let result = txt_of_stmts [] ast in
  List.iter print_endline result
