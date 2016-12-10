(* Semantic checking for the StockX compiler *)

open Ast
open Sast

type scope = {
	scope_name : string;
	scope_type : Ast.typ;	
}

type environ = {
	func_table : Sast.sfunc_decl list;
	symbol_table : Ast.bind;
	checked : Sast.sstatement list;
	env_scope : scope;
}


let init_environ = {
	func_table = built_in_decls;	
	symbol_table = [];
	checked = [];
	env_scope = SOMETHING;
}

(* find variable *)
(* check for variable duplicates and catch undefined variables *)
let rec check_var (scope:symbol_table) name =
try
 	List.find(fun(n,_) -> n = name)scope.vars; 
 	raise(Except("Variable '" ^ name ^ "' is already defined"))
 with Not_found -> sfname;

(* check if function has been defined *)
let rec find_func(functions: sfunc_decl list) name = 
	try 

	List.find(fun f -> f.sfname = name) functions
	with Not_found -> 	raise(Except("Function '" ^ name ^ "' has not been defined"))

(* check right side can be assigned to left side *)
let check_assign lvaltyp rvaltyp = 
	trylvaltyp == rvaltyp then raise (Except("Symbol '"^ ^"' should be of type "^lvaltyp ))

(* check variable declaration *)
let check_vdecl = 

in

(* add built in functions to function table *)
let built_in_decls = [
{
	fname = "map";
	formals = 
	typ = 
	body = 
};
{
	fname = "delta";
	formals = 
	typ = 
	body = 
};
{
	fname = "stddev";
	formals = [(Array, "timeseries")];
	typ = Float;
	body = [];
};
{
	fname = "correlation";
	formals = [(Array, "x");(Array, "y")];
	typ = Float;
	body = [];
};
{
	fname = "covariance";
	formals = 
	typ = 
	body = 
};
{
	fname = "regression";
	formals = 
	typ = 
	body = 
};
{
	fname = "emwa";
	formals = 
	typ = 
	body = 
};
{
	fname = "lma";
	formals = 
	typ = 
	body = 
}
]
(* check if function is defined *)
let check_func = 

(* check program *)
let check_prog



let check (func_decls, stmts) =

  (* Raise an exception if teh given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
      | n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet == rvaluet then lvaluet else raise err
  in

  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) func_decls)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) func_decls);




  ()

