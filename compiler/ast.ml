(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Mod | Addeq | Subeq | Multeq | Diveq | Modeq | And | Or | Dot | Bar

type uop = Neg | Not

type typ = Int | Float | Bool | Null | Void | Stock | Order | Portfolio | String | Array | Struct

type var_decl = {
  vname : string;
  vtyp : typ;
}

type array_decl = {
  aname : string;
  atyp : typ;
  asize : int;

}

type stock_decl = {
  sname : string;
}

type order_decl = {
  oname : string;

}

type portfolio_decl = {
  
}

type struct_decl = {
  
}

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

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Local of typ * string * expr
  | Array_Decl of array_decl
  | Array_Init of array_decl * expr list
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
  | Mod -> "%"
  | Addeq -> "+="
  | Subeq -> "-="
  | Multeq -> "*="
  | Diveq -> "/="
  | Modeq -> "%="
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"

  | Dot -> "."
  | Bar -> "|"


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
  | Structure -> "struct"
  | Function -> "func"
  
let rec string_of_expr = function
    StringLiteral(str) -> str
  | IntLiteral(i) -> "IntLiteral(" ^ string_of_int i ^ ")"
  | BoolLiteral(true) -> "true"
  | BoolLiteral(false) -> "false"
  | Id(s) -> s
  | Op(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | ObjAccess(e1, e2) -> string_of_expr e1 ^ "." ^ string_of_expr e2
  | Assign(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | _ -> "hurrdurr"


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
  | Local(t, id) -> string_of_typ t ^ id ^ ";"
  | Local(t, id, e) -> string_of_typ t ^ id ^ "=" ^ expr e ^ ";"
  | _ -> "hurrdurr"


let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.typ) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_stmt stmts) ^ "\n"

