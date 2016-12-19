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

  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty stmts in

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

    let add_local m (t, n) =
      let local_var = L.build_alloca (ltype_of_typ t) n builder
      in StringMap.add n local_var m
    in

    let rec get_locals mylocals = function
      | [] -> mylocals
      | [A.Local (t, s)] -> get_locals [(t, s)] []
      | A.Local (t, s) :: r -> get_locals ( (t, s) :: mylocals) r
      | _ :: r -> get_locals mylocals r
    in
    (* remember the values of arguments and local variables in the stmts map *)
    List.fold_left add_local StringMap.empty (get_locals [] stmts) in

  let lookup n = try StringMap.find n local_vars 
    with Not_Found -> StringMap.find n global_vars
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
    | A.ObjAccess (e1, e2) -> L.build_global_stringptr "Hi" "" builder
    | A.Noexpr -> L.build_global_stringptr "Hi" "" builder
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
    | A.Local (t, s) -> builder
  in

  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname and formal_types =
        Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.A.formals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types
      in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
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

    let builder = stmtgen builder (A.Block fdecl.A.body) in

    add_terminal builder (match fdecl.A.typ with
      | A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
  
  ignore (List.iter build_function_body func_decls);
  ignore (List.fold_left stmtgen builder stmts);
  ignore (L.build_ret (L.const_int i32_t 0) builder);
  the_module