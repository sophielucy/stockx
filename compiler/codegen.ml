module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (func_decls, stmts) =
  let context = L.global_context () in
  let the_module = L.create_module context "StockX"
  and i32_t  = L.i32_type   context
  and i8_t   = L.i8_type    context
  and i1_t   = L.i1_type    context in
  (* and void_t = L.void_type  context in *)

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each statement -> into a list ?? *)

  let fty = L.function_type i32_t [| |] in
  let f = L.define_function "main" fty the_module in
  let builder = L.builder_at_end context (L.entry_block f) in

  (* print formatters *)
  let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

  (* Construct code for an expression; return its value *)
  let rec exprgen builder = function
      A.IntLiteral i -> L.const_int i32_t i
    | A.BoolLiteral b -> L.const_int i1_t (if b then 1 else 0)
    | A.StringLiteral str -> L.build_global_stringptr str "" builder
    | A.Call ("print", [e]) -> let f e = match e with
        A.IntLiteral x -> L.build_call printf_func [| int_format_str; (exprgen builder e) |] "printf" builder
      | A.StringLiteral x -> L.build_call printf_func [| str_format_str; (exprgen builder e) |] "printf" builder
      in f e
    
    (*
    (match e
        with int x -> L.build_call printf_func [| int_format_str; (exprgen builder x) |] "printf" builder
        with string x -> L.build_call printf_func [| exprgen builder x |] "printf" builder


        ) e
    *)
    (*
    match e
        with int x -> L.build_call printf_func [| int_format_str; (exprgen builder e) |] "printf" builder
    *)

    (*
        L.build_call printf_func [| int_format_str ; (exprgen builder e) |]
        "printf" builder
    *)
    (* or another A.Call *)
    | _ -> L.build_global_stringptr "hi" "" builder
  in

  (* Build the code for the given statement; return the builder for
     the statement's successor *)
  let rec stmtgen builder = function
      A.Block sl -> List.fold_left stmtgen builder sl
    | A.Expr e -> ignore (exprgen builder e); builder
    | A.Return e -> ignore (exprgen builder e); builder
    | A.If (e, s1, s2) -> builder
    | A.For (e1, e2, e3, s) -> builder
    | A.While (e, s) -> builder
    | A.Local (typ, str, e) -> builder

  in
  
  ignore (List.fold_left stmtgen builder stmts);
  ignore (L.build_ret (L.const_int i32_t 0) builder);
  the_module
