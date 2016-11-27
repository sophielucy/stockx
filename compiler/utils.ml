open Ast
open Parser
open Processor

(* Pretty Printer *)

let save file string =
    let channel = open_out file in
    output_string channel string;
    close_out channel

let replace input output =
    Str.global_replace (Str.regexp_string input) output



(*
let token_list_to_string token_list =
  let rec helper = function
        (token, line) :: tail ->
        string_of_token_no_id token ^ " " ^ helper tail
    |   [] -> "\n"
  in helper token_list
  *)
(* Print data types *)





(*
let rec fdecls_tree = function
        [] -> ""
    |   [fdecl] ->  "\n\t\tfunction " ^ fdecl.fname ^ "\n"
    |   fdecl :: tail ->
            "\n\t\trunction " ^ fdecl.fname ^ "\n" ^


let print_tree = function
    Program(fdecls, stmts) ->
        "[program:\n" ^
            "\t[fdecls:" ^ fdecls_tree fdecls ^ "]\n" ^
            "\t[stmts:" ^ stmts_tree stmts ^ "]\n" ^
        "]\n"
        *)
