(* Code Generator for MATHLang
 *)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (func_decls, stmts) =
  let context = L.global_context () in
  let the_module = L.create_module context "StockX"
  and i32_t    = L.i32_type   context
  and i8_t     = L.i8_type    context
  and i1_t     = L.i1_type    context
  and void_t   = L.void_t     context
  and double_t = L.double_type context in

  let ltype_of_typ = function
    | A.Int -> i32_t
    | A.Float -> double_t
    | A.Bool -> i1_t
    | A.Void -> void_t
    | _ -> i32_t in


  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* build the entry point for the llvm file *)
  let fty = L.function_type i32_t [| |] in
  let f = L.define_function "main" fty the_module in
  let builder = L.builder_at_end context (L.entry_block f) in

  (* print formatters *)
  let str_format_str   = L.build_global_stringptr "%s\n" "fmt" builder in
  let int_format_str   = L.build_global_stringptr "%d\n" "fmt" builder in
  let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in

  let local_vars =
    let add_formal m (t, n) p = L.set_value_name n p;
    let local = L.build_alloca (ltype_of_typ t) n builder in
    ignore (L.build_store p local builder);
    StringMap.add n local m in

    let add_local m (t, n) =
    let local_var = L.build_alloca (ltype_of_typ t) n builder
    in StringMap.add n local_var m in

    let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
        (Array.to_list (L.params the_function)) in
    List.fold_left add_local formals fdecl.A.locals in

   (*  let add_local m (t, n) =
      let local_var = L.build_alloca (ltype_of_typ t) n builder
      in StringMap.add n local_var m
    in

    let rec get_locals mylocals = function
        [] -> mylocals
      | [A.Local (t, s)] -> get_locals [(t, s)]
      | A.Local (t, s) :: r -> get_locals ( (t, s) :: mylocals) r
      | _ :: r -> get_locals mylocals r
    in *)
    (* remember the values of arguments and local variables in the stmts map *)
(*     List.fold_left add_local StringMap.empty (get_locals [] stmts) in
 *)

  let lookup n = StringMap.find n local_vars 
  in

  (* Construct code for an expression; return its value *)
  let rec exprgen builder = function
    | A.IntLiteral i -> L.const_int i32_t i
    | A.FloatLiteral f -> L.const_float double_t f
    | A.StringLiteral str -> L.build_global_stringptr str "" builder
    | A.BoolLiteral b -> L.const_int i1_t (if b then 1 else 0)
    | A.Id s -> L.build_load (lookup s) s builder

    (* e1 and e2 are always same type -> do in semantic checker *)
    | A.Binop (e1, op, e2) ->
      let e1' = exprgen builder e1
      and e2' = exprgen builder e2 in
      (match op with
        | A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
      ) e1' e2' "tmp" builder
        
    | A.Unop (uop, e) -> let e' = exprgen builder e in
        (match uop with
          | A.Neg   -> L.build_neg
          | A.Not   -> L.build_not
        ) e' "tmp" builder
    | A.Assign (s, e) -> let e' = exprgen builder e in
                         ignore (L.build_store e' (lookup s) builder); e'
    | A.Call ("print", [e]) -> let func e =
                                 let f = (exprgen builder e) in
                               match e with
    | A.IntLiteral x -> L.build_call printf_func
                        [| int_format_str; (exprgen builder e) |]
                        "printf" builder
    | A.StringLiteral x -> L.build_call printf_func
                           [| str_format_str; (exprgen builder e) |]
                           "printf"
                           builder
    | A.FloatLiteral x -> L.build_call printf_func
                          [| float_format_str; (exprgen builder e) |]
                          "printf" builder
    | A.BoolLiteral x -> let boolfunc b = match string_of_bool b with
            | "true" -> L.build_call printf_func
                        [| str_format_str;
                           (L.build_global_stringptr "'true'" "" builder)
                        |] "printf" builder
            | "false" -> L.build_call printf_func
                         [| str_format_str;
                            (L.build_global_stringptr "'false'" "" builder)
                         |] "printf" builder
            | _ -> L.build_global_stringptr "---error---" "" builder
            in boolfunc x
      | _ -> L.build_call printf_func
                          [| int_format_str; f |]
                          "printf" builder
      in func e
    (* or another A.Call *)
    | A.Call (str, el) -> L.build_global_stringptr "Hi" "" builder
    | A.Noexpr -> L.build_global_stringptr "Hi" "" builder
  in

  (* Build the code for the given statement; return the builder for
     the statement's successor *)
  let rec stmt builder = function
      A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
        A.Void -> L.build_ret_void builder
      | _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
        let bool_val = expr builder predicate in
        let merge_bb = L.append_block context "merge" the_function in

   let then_bb = L.append_block context "then" the_function in
   add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
     (L.build_br merge_bb);

   let else_bb = L.append_block context "else" the_function in
   add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
     (L.build_br merge_bb);

   ignore (L.build_cond_br bool_val then_bb else_bb builder);
   L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
    let pred_bb = L.append_block context "while" the_function in
    ignore (L.build_br pred_bb builder);

    let body_bb = L.append_block context "while_body" the_function in
    add_terminal (stmt (L.builder_at_end context body_bb) body)
      (L.build_br pred_bb);

    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = expr pred_builder predicate in

    let merge_bb = L.append_block context "merge" the_function in
    ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> stmt builder
      ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname and formal_types =
        Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.A.formals) in
      let ftype = L.function_type (ltype_of_typ fdecl.A.ftyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty func_decls
  in

  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        | Some _ -> ()
        | None -> ignore (f builder)
    in

    let builder = stmt builder (A.Block fdecl.A.body) in

    add_terminal builder (match fdecl.A.typ with
      | A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
  
  ignore (List.iter build_function_body func_decls);
  ignore (List.fold_left stmt builder stmts);
  ignore (L.build_ret (L.const_int i32_t 0) builder);
  the_module