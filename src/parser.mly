%{  open Ast  %}

%token INT FLOAT BOOL CHAR VOID NULL TRUE FALSE
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA DOT
%token AND NOT OR PLUS MINUS TIMES DIVIDE ASSIGN MODULO
%token EQ NEQ LT LEQ GT GEQ BAR
%token IF ELSE FOR WHILE RETURN
%token STOCK ORDER PORTFOLIO
%token FUNCTION
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <char> CHAR_LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
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
    FUNCTION ID LPAREN formals_opt

formals_opt:
    /* nothing */   { [] }
|   formal_list     { List.rev $1 }

formal_list:
    typ ID                      { [($1, $2)] }
|   formal_list COMMA typ ID    { ($3, $4) :: $1 }

typ:
    INT     { Int }
|   FLOAT   { Float }
|   CHAR    { Char }
|   BOOL    { Bool }
|   VOID    { Void }

