%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <char> CHAR
%token <bool> BOOL
%token <string> ATOM
%token UNIT

%token <Ast.prim> TY_PRIM
(* %token <Ast.generic> TY_GENERIC *)

%token <string> IDENT
%token <string> BUILTIN_FUNC

%token WHEN WITH WITHOUT
%token IF THEN ELSE
%token DEC DEF LET
%token IMPORT MODULE

%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token SEMI SEMISEMI COLON COLONCOLON
%token FUNCTION_ARROW
%token EQUALS DEF_EQUALS
%token ATSIGN DOT COMMA
%token EOF

%start <Ast.program list> program
%%

program:
  | mod = MODULE; defs = list(toplvl_defs); EOF {(mod, defs)} ;

toplvl_defs:
  | ATSIGN IMPORT {import_statement}
  | DEC i = IDENT COLON typs = dec_sig {Ast.TDef @@ Ast.Dec (i, typs)}
  | DEF i = IDENT args = list(IDENT) DEF_EQUALS body = terms SEMISEMI {}

import_statement:
  | i = IDENT WITHOUT excludes = conditional_imports 
    {(i, Some @@ CWithout excludes)}
  | i = IDENT WITH includes = conditional_imports
    {(i, Some @@ CWith includes)}
  | i = IDENT 
    {(i, None)} ;

conditional_imports:
  | i = IDENT {[i]}
  | cs = delimited(LPAREN, separated_list(COMMA, IDENT), RPAREN) {cs} ;

dec_sig:
  | t = typ {[t]}
  | ts = separated_list(FUNCTION_ARROW, typ) {ts} ;

typ:
  | t = TY_PRIM 
    {Ast.Prim t}
  | i = IDENT t = list(typ) 
    {Ast.Constructor (i, t)}
  | LBRACKET t = typ RBRACKET
    {Ast.List t}
  | LPAREN ts = separated_nonempty_list(COMMA, typ) RPAREN 
    {Ast.Tuple ts} ;
