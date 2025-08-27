%token <int> INT
%token <float> FLOAT
%token <string> STRING ATOM
%token <char> CHAR
%token <bool> BOOL
%token UNIT

%token <Ast.prim> TY_PRIM
%token <string> IDENT UPPER_IDENT
%token <string> OP

%token WHEN WITH WITHOUT
%token IF THEN ELSE
%token DEC DEF LET IN
%token MODULE IMPORT

%token AND NOT OR
%token LT GT LTE GTE NE
%token PLUS MINUS
%token DIV MUL CONS

%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token SEMI SEMISEMI COLON
%token ARROW
%token EQ ASSIGNMENT
%token ATSIGN DOT COMMA BTICK
%token EOF

%start <Ast.program> program
%%

program:
  | mod_name = module_def; defs = list(toplvl_defs); EOF {(mod_name, defs)} ;

%inline module_def:
  | ATSIGN MODULE i = UPPER_IDENT {i} ;

toplvl_defs:
  | ATSIGN imp = import_statement
    {Ast.TImport imp}
  | DEC d = dec_sig | DEF d = def_statement
    {Ast.TDef d} ;

import_statement:
  | i = UPPER_IDENT WITHOUT excludes = conditional_imports 
    {(i, Some (CWithout excludes))}
  | i = UPPER_IDENT WITH includes = conditional_imports
    {(i, Some (CWith includes))}
  | i = UPPER_IDENT 
    {(i, None)} ;

conditional_imports:
  | i = IDENT {[i]}
  | cs = delimited(LPAREN, separated_list(COMMA, IDENT), RPAREN) {cs} ;

dec_sig:
  | i = IDENT COLON t = typ DOT {Ast.Dec (i, [t])}
  | i = IDENT COLON ts = separated_list(ARROW, typ) DOT {Ast.Dec (i, ts)} ;

def_statement:
  | i = IDENT args = list(IDENT) COLON WHEN cond = term EQ body = nonempty_list(term) WITH local_defs = flatten(list(with_block)) SEMISEMI
    {Ast.Def (i, args, (Some cond), body, (Some local_defs))}
  | i = IDENT args = list(IDENT) COLON WHEN cond = term EQ body = nonempty_list(term) SEMISEMI 
    {Ast.Def (i, args, (Some cond), body, None)}
  | i = IDENT args = list(IDENT) ASSIGNMENT body = nonempty_list(term) WITH local_defs = flatten(list(with_block)) SEMISEMI
    {Ast.Def (i, args, None, body, (Some local_defs))}
  | i = IDENT args = list(IDENT) ASSIGNMENT body = nonempty_list(term) SEMISEMI 
    {Ast.Def (i, args, None, body, None)} ; 

with_block:
  | DEC dsig=dec_sig DEF def=def_statement {[dsig; def]}
  | DEF def=def_statement {[def]} ;

term:
  | LET i = IDENT COLON t = typ EQ v = term IN
    {Ast.TLet (i, (Some t), v)}
  | LET i = IDENT ASSIGNMENT v = term IN
    {Ast.TLet (i, None, v)}
  | body = delimited(LBRACE, nonempty_list(term), RBRACE) 
    {Ast.TGrouping body}
  | IF cond = term THEN tbranch = term ELSE fbranch = term
    {Ast.TIf (cond, tbranch, Some fbranch)}
  | IF cond = term THEN tbranch = term
    {Ast.TIf (cond, tbranch, None)}
  | i = prefix_ident args = list(arg) 
    {Ast.TAp (Ast.Prefix (i, args))}
  | l = arg i = infix_ident r = arg 
    {Ast.TAp (Ast.Infix (l, i, r))}
  | l = lit 
    {Ast.TLit l} ;

prefix_ident:
  | i = IDENT {i}
  | LPAREN o = op RPAREN {o} ;

infix_ident:
  | BTICK i = IDENT BTICK {i}
  | o = op {o} ;

arg:
  | l = lit 
    {Ast.ALit l}
  | i = IDENT 
    {Ast.AIdent i}
  | i = prefix_ident  args = list(arg)
    {Ast.AAp (Ast.Prefix (i, args))}
  | l = arg i = infix_ident r = arg
    {Ast.AAp (Ast.Infix (l, i, r))} ;

%inline op:
  | o = OP 
    {o}
  | AND 
    {"&&"}
  | OR 
    {"||"}
  | NOT 
    {"!"}
  | LT 
    {"<"}
  | GT 
    {">"}
  | LTE 
    {"<="}
  | GTE 
    {">="}
  | NE 
    {"/="}
  | PLUS 
    {"+"}
  | MINUS 
    {"+"}
  | DIV 
    {"/"}
  | MUL 
    {"*"}
  | CONS 
    {"::"} ;

%inline lit:
  | i = INT 
    {Ast.Int i}
  | f = FLOAT 
    {Ast.Float f}
  | c = CHAR 
    {Ast.Char c}
  | s = STRING 
    {Ast.String s}
  | b = BOOL
    {Ast.Bool b}
  | ATSIGN i = IDENT 
    {Ast.Atom i}
  | UNIT 
    {Ast.Unit} ;

typ:
  | t = TY_PRIM 
    {Ast.Prim t}
  | i = IDENT t = nonempty_list(typ) 
    {Ast.Constructor (i, t)}
  | LBRACK t = typ RBRACK
    {Ast.List t}
  | LPAREN ts = separated_nonempty_list(COMMA, typ) RPAREN 
    {Ast.Tuple ts} ;
