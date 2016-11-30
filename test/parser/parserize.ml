open Ast

let txt_of_op = function
  | Add -> "Add"
  | Sub -> "Sub"
  | _ -> "not-add-or-sub"

let rec txt_of_expr = function
  | IntLiteral(i) -> "IntLiteral(" ^ string_of_int i ^ ")"
  | Binop(e1, op, e2) ->
      let v1 = txt_of_expr e1
      and v2 = txt_of_expr e2
      and oper = txt_of_op op in
      "Binop(" ^ v1 ^ ", " ^ oper ^ ", " ^ v2 ^ ")"
  | _ -> "hurrdurr"

let rec txt_of_stmt = function
  | Expr(expr) -> txt_of_expr expr
  | _ -> "Wow"

let rec txt_of_stmts stor = function
  | [] -> (String.concat " ; " stor)
  | stmt :: tl -> txt_of_stmts (txt_of_stmt stmt :: stor) tl

let txt_of_program stor (func_decls, stmts) =
  txt_of_stmts stor stmts 

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  let result = txt_of_program [] ast in
  print_endline result
