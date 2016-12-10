(* Abstract Syntax Tree and functions for printing them for StockX *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type uop = Neg | Not

type typ = Int | Float | Bool | Null | Void | Stock | Order | Portfolio | String | Array | Struct 

type var_decl = {
  vname : string;
  vtyp : typ;
}

(* type stock_decl = {
  sname : string;
}

type order_decl = {
  oname : string;

}

type portfolio_decl = {
  
}

type struct_decl = {
  
} *)

type expr =
    IntLiteral of int
  | FloatLiteral of float
  | StringLiteral of string
  | BoolLiteral of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | ObjAccess of expr * expr
  | Array_Assign of string * expr * expr 
  | Array_Access of string * expr
 
  (* stock assign
    stock access
    order assign
    order access
    portfolio assign
    portfolio access
    struct assign
    struct access
  *)
  | Noexpr

type array_decl = { 
  atyp  : typ;    
  aname : string;
  asize : expr;
}

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Array_Decl of array_decl
  | Array_Init of array_decl * expr list
  | V_Decl of var_decl
  (* stock decl
    stock init
    order decl
    order init
    portfolio decl
    portfolio init
    struct decl 
    struct init
  *)

type func_decl = {
  fname : string;
  formals : var_decl list;
  ftyp : typ;
  body : stmt list;
}

type program = func_decl list * stmt list

(* Pretty printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"


let string_of_uop = function
    Neg -> "-"
  | Not -> "not"

let string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Void -> "void"
  | Stock -> "stock"
  | Order -> "order"
  | Portfolio -> "portfolio"
  | String -> "string"
  | Array -> "array"
  | Struct -> "struct"
  | Null -> "null"
  
let rec string_of_expr = function
    StringLiteral(str) -> str
  | FloatLiteral(f) -> "FloatLiteral("^ string_of_float f ^")" 
  | IntLiteral(i) -> "IntLiteral(" ^ string_of_int i ^ ")"
  | BoolLiteral(true) -> "true"
  | BoolLiteral(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | ObjAccess(e1, e2) -> string_of_expr e1 ^ "." ^ string_of_expr e2
  | Assign(a, e) -> a ^ " = " ^ string_of_expr e
  | Array_Assign(id, index, e) -> id ^ "[" ^ string_of_expr index ^"] = " ^ string_of_expr e
  | Array_Access(id, index) -> id ^ "[" ^ string_of_expr index ^ "]"
  | Call(f, e) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"
  | Noexpr -> ""

let string_of_vdecl v = string_of_typ v.vtyp ^ " " ^ v.vname ^ ";\n"

let string_of_array_decl array_decl = string_of_typ array_decl.atyp ^ " " ^ 
	array_decl.aname ^ "[" ^ string_of_expr array_decl.asize ^ "]"		

let string_of_arraylist list = "[" ^ String.concat ", " (List.map string_of_expr list) ^ "]"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Array_Decl(aname) -> string_of_array_decl aname ^ ";\n"		
  | Array_Init(aname, list) -> string_of_array_decl aname ^ " = " ^ string_of_arraylist list ^ ";\n"
  | V_Decl(v) -> " ^ string_of_vdecl v ^ ";"
  | V_Decl(typ, id, e) -> string_of_typ typ ^ id ^ "=" ^ expr e ^ ";"

let string_of_fdecl fdecl =
  string_of_typ fdecl.ftyp ^ " " ^ fdecl.fname ^ "(" ^ 
  String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (stmts, funcs) =
  String.concat "" (List.map string_of_stmt stmts) ^ "\n" ^ 
  String.concat "\n" (List.map string_of_fdecl funcs)

