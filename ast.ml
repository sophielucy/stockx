(* MATHLANG Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type uop = Neg | Not

type typ = Int | Bool | Void | Float | String | Array | Struct

type var_decl = {
  vtyp  : typ;
  vname : string;
}

type field = Field of typ * string

type expr =
    IntLiteral of int
  | FloatLiteral of float
  | StringLiteral of string
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Array_Assign of string * expr * expr
  | Array_Access of string * expr
  | Struct_Assign of string * expr * expr
  | Struct_Access of string * expr
  | Noexpr

type array_decl = {
  atyp  : typ;
  aname : string;
  asize : expr;
}

type struct_decl = {
  sname : string;
  fields : field list;
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
  | Struct_Decl of struct_decl 
  | Struct_Init of struct_decl * field list
  | V_Decl of var_decl
  | V_Assign of var_decl * expr

type func_decl = {
    fname : string;
    formals : var_decl list;
    ftyp : typ;
    body : stmt list;
  }

type program = func_decl list * stmt list

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
  | String -> "string"
  | Array -> "array"
  | Struct -> "struct"

let rec string_of_expr = function
    StringLiteral(str) -> str
  | FloatLiteral(f) -> "FloatLiteral("^ string_of_float f ^")"
  | IntLiteral(i) -> "IntLiteral(" ^ string_of_int i ^ ")"
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Array_Access(id, index) -> id ^ "[" ^ string_of_expr index ^ "]"  
  | Array_Assign(id, index, e) -> id ^ "[" ^ string_of_expr index ^"] = " ^ string_of_expr e
  | Struct_Assign(id, index_name, e) -> id ^ "." ^ string_of_expr index_name ^ "=" ^ string_of_expr e
  | Struct_Access(id, index_name) -> id ^ "." ^ string_of_expr index_name
  | Noexpr -> ""

let string_of_vdecl v = string_of_typ v.vtyp ^ " " ^ v.vname ^ ";\n"

let string_of_array_decl array_decl = "array " ^ string_of_typ array_decl.atyp ^ " " ^
        array_decl.aname ^ "[" ^ string_of_expr array_decl.asize ^ "]"

let string_of_arraylist list = "[" ^ String.concat ", " (List.map string_of_expr list) ^ "]"

let string_of_field = function
  Field(t, id)-> string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_struct_decl struct_decl = "struct " ^ struct_decl.sname ^ " = { " ^ List.map string_of_field list ^ " }"

let string_of_struct_list list = "{" ^ String.concat "" (List.map string_of_field list) ^ "}"

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
  | Struct_Decl(struct_name) -> string_of_struct_decl struct_name ^ ";\n"
  | Struct_Init(struct_name, list) -> string_of_struct_decl struct_name ^ " = " ^ string_of_struct_list list ^ ";\n"
  | V_Decl(v) -> string_of_vdecl v ^ ";\n"
  | V_Assign(v, e) -> string_of_vdecl v ^ " = " ^ string_of_expr e ^ ";\n"
 

let string_of_fdecl fdecl =
  fdecl.fname ^ 
  "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")" ^ 
  string_of_typ fdecl.ftyp ^ "\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (stmts, funcs) =
  String.concat "" (List.map string_of_stmt stmts) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
