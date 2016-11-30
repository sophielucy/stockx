(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal |
          Neq | Less | Leq | Greater | Geq | And | Or

type uop = Neg | Not

type typ = Int | Float | Bool | Void | Stock | Order |
           Portfolio | String | Array | Struct

type bind = typ * string

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
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Local of typ * string * expr

type func_decl = {
    fname : string;
    formals : bind list;
	typ : typ;
    body : stmt list;
  }

type program = func_decl list * stmt list

(* Pretty printing functions *)
let rec string_of_expr = function
  | IntLiteral(i) -> "IntLiteral(" ^ string_of_int i ^ ")"
  | StringLiteral(str) -> str
  | Call(str, el) ->
      str ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | _ -> "hurrdurr"

let rec string_of_stmt = function
    Expr(expr) -> string_of_expr expr
  | _ -> "hurrdurr"

let string_of_program (fdecls, stmts) =
  String.concat "" (List.map string_of_stmt stmts) ^ "\n"
