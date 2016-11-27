type action = Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-c", Compile) ] (*Generate, check LLVM IR*)
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  (* ERROR in line 10 *)
  print_endline "1\n";
  let ast = Parser.program Scanner.token lexbuf in
  Semant.check ast;
  print_endline "3\n";
  match action with
    Compile -> let m = Codegen.translate ast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)
