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
%token EQUALS ASSIGNMENT
%token ATSIGN DOT COMMA
%token EOF

%start <Ast.program list> program
%%

program:
  | mod = MODULE; defs = list(toplvl_defs); EOF {(mod, defs)} ;

toplvl_defs:
  | ATSIGN IMPORT 
    {import_statement}
  | DEC 
    {dec_sig}
  | DEF 
    {def_statement}

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
  | i = IDENT COLON t = typ {Ast.TDef @@ Ast.Dec (i, [t])}
  | i = IDENT COLON ts = separated_list(FUNCTION_ARROW, typ) {Ast.TDef @@ Ast.Dec (i, ts)} ;

def_statement:
  | i = IDENT args = list(IDENT) COLON cond = when_block EQUALS body = nonempty_list(term) WITH local_defs = flatten(list(with_block)) 
    {Ast.TDef @@ Ast.Def (i, args, (Some cond), body, (Some local_defs))}
  | i = IDENT args = list(IDENT) COLON cond = when_block EQUALS body = nonempty_list(term) SEMISEMI 
    {Ast.TDef @@ Ast.Def (i, args, (Some cond), body, None)}
  | i = IDENT args = list(IDENT) ASSIGNMENT body = nonempty_list(term) WITH local_defs = flatten(list(with_block)) 
    {Ast.TDef @@ Ast.Def (i, args, None, body, (Some with_block))}
  | i = IDENT args = list(IDENT) ASSIGNMENT body = nonempty_list(term) SEMISEMI 
    {Ast.TDef @@ Ast.Def (i, args, None, body, None)} ; 

when_block:
  | WHEN LBRACE cond = nonempty_list(term) RBRACE {cond}
  | WHEN cond = term {cond} ;

with_block:
  | DEC dsig=dec_sig DEF def=def_statement {[dsig, def]}
  | DEF def=def_statement {[def]} ;

term:
  | LET i = IDENT COLON t = typ EQUALS v = term {Ast.TLet (i, (Some t), v)}
  | LET i = IDENT ASSIGNMENT v = term {Ast.TLet (i, None, v)}
  | body = delimited(LBRACE, nonempty_list(term), RBRACE) {Ast.TGrouping body}
  | lit' = lit {Ast.TLit lit'}

lit:
  | i = INT {Ast.Int i}
  | f = FLOAT {Ast.Float f}
  | c = CHAR {Ast.Char c}
  | s = STRING {Ast.String s}
  | b = BOOL {Ast.Bool b}
  | ATSIGN i = IDENT {Ast.Atom i}
  | UNIT {Ast.Unit} ;

typ:
  | t = TY_PRIM 
    {Ast.Prim t}
  | i = IDENT t = list(typ) 
    {Ast.Constructor (i, t)}
  | LBRACKET t = typ RBRACKET
    {Ast.List t}
  | LPAREN ts = separated_nonempty_list(COMMA, typ) RPAREN 
    {Ast.Tuple ts} ;
