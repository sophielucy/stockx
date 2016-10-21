%{  open Ast  %}

%token INT FLOAT BOOL VOID NULL TRUE FALSE
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA DOT
%token AND NOT OR PLUS MINUS TIMES DIVIDE ASSIGN MODULO
%token PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODULOEQ
%token EQ NEQ LT LEQ GT GEQ BAR
%token IF ELSE FOR WHILE RETURN
%token STOCK ORDER PORTFOLIO
%token FUNCTION
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> ID
%token <string> STRING
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS PLUSEQ MINUSEQ
%left TIMES DIVIDE MODULO TIMESEQ DIVIDEEQ MODULOEQ
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
    decls EOF { $1 }

decls:
    /* nothing */   { [], [] }
|   decls fdecl     { ($2 :: fst $1), snd $1 }
|   decls statement { fst $1, ($2 :: snd $1) }

fdecl:
    FUNCTION ID LPAREN formals_opt RPAREN RETURN LPAREN typ RPAREN LBRACE stmnt_list RBRACE

formals_opt:
    /* nothing */   { [] }
|   formal_list     { List.rev $1 }

formal_list:
    typ ID                      { [($1, $2)] }
|   formal_list COMMA typ ID    { ($3, $4) :: $1 }

typ:
    INT     { Int }
|   FLOAT   { Float }
|   BOOL    { Bool }
|   VOID    { Void }
|   STOCK   { Stock }
|   ORDER   { Order }
|   PORTFOLIO   { Portfolio }
|   STRING  { String }
|   ARRAY   { Array }
|   STRUCT  { Struct }
|   FUNCTION    { Function }

stmnt_list:
    
