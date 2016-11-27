open Ast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.
*)

let check (func_decls, stmts) =
  List.iter print_endline ["good"; "bye"]
