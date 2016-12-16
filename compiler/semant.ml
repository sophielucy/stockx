(* Semantic checking for the StockX compiler *)

open Ast
open Sast
exception Error of string

type scope = {
	scope_name : string;
	scope_type : Ast.typ;
	return_typ : void;
(* 	in_func = false;	
 *)}

type environ = {
	func_table : Sast.sfunc_decl list;
	symbol_table : Ast.var_decl list;
	checked_statements : Sast.sstatement list;
	env_scope : scope;
	returned = bool;
}

let init_env = {
	func_table = [];	
	symbol_table = [];
	checked_statements = [];
	env_scope = {};
	returned = false;
}

(* let init_scope = { parent = None; vars = [];} in
{
	scope = init_scope;
	func_table = [];	
	symbol_table = [];
	checked_statements = [];
} *)

(* check for global variable name duplicates *)


(* check for argument name duplicates *)
let check_formals sformals (formal: var_decl) = 
let found = List.exists (fun sf -> formal.vname = sf.sfname) sformals in
	if found then raise (Except("Formal parameter '" ^ formal.vname ^ "' is already defined!")) 
	else formal :: sformals

(* check for function name duplicates *)
let check_functions env func_name=
try ignore (List.find (fun f -> f.fname = func_name) environ.func_table; 
	raise (Except("Function '" ^ func_name ^ "' is already defined!"))
	with Not_found -> sfname)

(* check for duplicates in symbol table and catch undefined variables else add to symbol table *)
let rec check_var env (vdecl: var_decl) = 
	let found = List.exists (fun symbol -> symbol.vname = vdecl.vname) env.symbol_table in
	if found then raise (Except("Variable '" ^ vdecl.vname ^ "' is already defined!"))
	else vdecl


(* check left and right of binary operations for type error *)
let check_binop (ltyp: typ) (rtyp: typ) env = 
match (lhs, rhs) with
	  (Int, Int) 		-> Int
	| (Float, Float) 	-> Float
	| (_, _) -> raise (Error("Binary operations can only be of type int or type float"))

(* (* check variable has been initialised *)
let check_vdecl vdecl_name env =
	try 
	let variable = List.find(fun s -> s.vname = vdecl_name)env.symbol_table in
		{ sexpr = Svar(name); sdtype = var.dtype; }
	with Not_found -> raise (Except("Symbol '" ^ name ^ "' is uninitialized!")) 
 *)

(* type check expressions *)
let rec check_expr (e:expr) env = 
	match e with
	  	IntLiteral(i) 	-> SIntLiteral(i), Int
	  | FloatLiteral(f) -> SFloatLiteral(f), Float
	  | StringLiteral(s)-> SStringLiteral(s), String
	  | BoolLiteral(b) 	-> SBoolLiteral(b), Bool
(* 	  check for declaration and get name and type*)
	  | Id -> let var = 
	  | Binop(lhs, binop, rhs) -> check_ret_binop lhs binop rhs
	  | Unop(unop, rhs) -> check_unop rhs unop env 
	  | Assign(lhs, rhs) -> check_assign lhs rhs env 
	  | Call(func, el) -> check_func_call func el env
	  | Array_Assign(name, index, e1) -> check_array_assign name index e1 env
	  | Array_Access(name, index) -> check_array_access name index env
	  | Noexpr -> SNoexpr, Void 
	  | _ -> SNoexpr, Void
 
(* check binop return type *)
and check_ret_typ (lhs: expr)(operator: binop)(rhs: expr) env = 

match operator with 
	  Add -> SBinop(lhs, operator, rhs), check_binop t1 t2 env
	| Sub -> SBinop(lhs, operator, rhs), check_binop t1 t2 env
	| Mult -> SBinop(lhs, operator, rhs), check_binop t1 t2 env
	| Div -> SBinop(lhs, operator, rhs), check_binop t1 t2 env
	| Equal -> SBinop(lhs, operator, rhs), check_binop t1 t2 env
	| Neq -> SBinop(lhs, operator, rhs), check_binop t1 t2 env
	| Less -> SBinop(lhs, operator, rhs), check_binop t1 t2 env
	| Leq -> SBinop(lhs, operator, rhs), check_binop t1 t2 env
	| Greater -> SBinop(lhs, operator, rhs), check_binop t1 t2 env
	| Geq -> SBinop(lhs, operator, rhs), check_binop t1 t2 env
	| And -> SBinop(lhs, operator, rhs), check_binop t1 t2 env
	| Or-> SBinop(lhs, operator, rhs), check_binop t1 t2 env

and check_unop (rhs:expr) (operator: unop) env = 
let typ = rhs.styp;
match typ with
	  Int -> SUnop(operator, rhs), Int
	| Float -> SUnop(operator, rhs), Float
	| t -> raise (Except("Unary operations are not supported for type '" ^ Ast.string_of_typ typ ^ "'!"))		

(* check right side can be assigned to left side in variable declarations *)
and check_assign (lhs: vdecl)(rhs: expr) env = 


and check_func_call (func_name: string) (el: expr list) env =


and check_array_assign (array_name: string) (index: expr) (e1: expr) env =

and check_array_access (array_name: string) (index: expr) =

(* add built in functions to function table *)
let built_in_decls = Stringmap.add[
{
	fname = "print_int";
	formals = [(vtyp = Int,vname = "x")];
	typ = Void;
	body = [];
};
{
	fname = "print_str";
	formals = [(vtyp = String, vname = "x")];
	typ = Void;	
	body = [];
};
]
in
