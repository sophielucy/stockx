open Ast

type sexpr =
    SIntLiteral of int
  | SFloatLiteral of float
  | SStringLiteral of string
  | SBoolLiteral of bool
  | SId of string
  | SBinop of expr * op * expr
  | SUnop of uop * expr
  | SAssign of string * expr
  | SCall of string * expr list
  | SObjAccess of expr * expr
  | SArray_Assign of string * expr * expr 
  | SArray_Access of string * expr
  | SNoexpr

type svdecl = {
  svname : string;
  styp : typ;
}

type sstmt =
    SBlock of stmt list
  | SExpr of expr
  | SReturn of expr
  | SIf of expr * stmt * stmt
  | SFor of expr * expr * expr * stmt
  | SWhile of expr * stmt
  | SLocal of typ * string * expr
  | SArray_Decl of array_decl
  | SArray_Init of array_decl * expr list

type sfunc_decl = {
  sfname : string;
  sformals : bind list;
	sftyp : typ;
  sbody : sstmt list;
}

type sprogram = sfunc_decl list * sstmt list