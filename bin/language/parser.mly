%token <int> INT
%token <float> FLOAT
%token <string> STRING
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
%token EQ ASSIGNMENT ARROW
%token ATSIGN DOT COMMA BTICK WILDCARD
%token EOF

%left EQ
%left AND OR
%right CONS
%left PLUS MINUS
%left MUL DIV

%start <Ast.program> program
%%

program:
  | mod_name = module_def; defs = list(toplvl_defs); EOF {(mod_name, defs)} ;

%inline module_def:
  | ATSIGN MODULE i = UPPER_IDENT {i} ;

toplvl_defs:
  | ATSIGN IMPORT imp = import_statement
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

%inline dec_sig:
  | i = IDENT COLON t = nonempty_list(typ) DOT {Ast.Dec (i, t)}

def_statement:
  | i = IDENT args = list(func_arg) COLON WHEN cond = term EQ body = term_list WITH local_defs = flatten(list(with_block)) SEMISEMI
    {Ast.Def (i, args, (Some cond), body, (Some local_defs))}
  | i = IDENT args = list(func_arg) COLON WHEN cond = term EQ body = term_list SEMISEMI 
    {Ast.Def (i, args, (Some cond), body, None)}
  | i = IDENT args = list(func_arg) ASSIGNMENT body = term_list WITH local_defs = flatten(list(with_block)) SEMISEMI
    {Ast.Def (i, args, None, body, (Some local_defs))}
  | i = IDENT args = list(func_arg) ASSIGNMENT body = term_list SEMISEMI 
    {Ast.Def (i, args, None, body, None)} ; 

func_arg:
  | i = IDENT {Ast.ArgIdent i}
  | l = lit   {Ast.ArgMatch (Ast.MLit l)}
  | WILDCARD  {Ast.ArgMatch Ast.MWild}
  | LBRACK RBRACK {Ast.ArgMatch Ast.MList}
  | LPAREN l = func_arg CONS r = func_arg RPAREN {Ast.ArgMatch (Ast.MCons (l, r))}

with_block:
  | DEC dsig=dec_sig DEF def=def_statement {[dsig; def]}
  | DEF def=def_statement {[def]} ;

term_list:
  | t = term {[t]}
  | t = expr SEMI ts = term_list | t = term ts = term_list {t :: ts}

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
  | LPAREN es = separated_nonempty_list(COMMA, list_item) RPAREN
    {Ast.TTup es}
  | e = expr
    {e}

expr:
  | i = prefix_ident args = nonempty_list(ap_arg)
    {Ast.TAp (Ast.Prefix (i, args))}
  | l = ap_arg i = infix_ident r = ap_arg
    {Ast.TAp (Ast.Infix (l, i, r))}
  | LBRACK items = separated_list(SEMI, list_item) RBRACK
    {Ast.TList items}
  | l = lit
    {Ast.TLit l} ;

prefix_ident:
  | i = IDENT {i}
  | u = unop | u = OP {u}
  | LPAREN o = binop RPAREN | LPAREN o = OP RPAREN {o} ;

infix_ident:
  | BTICK i = IDENT BTICK {i}
  | o = binop | o = OP {o} ;

ap_arg:
  | l = lit 
    {Ast.ALit l}
  | i = IDENT 
    {Ast.AIdent i}
  | LPAREN i = prefix_ident args = nonempty_list(ap_arg) RPAREN
    {Ast.AAp (Ast.Prefix (i, args))}
  | LPAREN l = ap_arg i = infix_ident r = ap_arg RPAREN
    {Ast.AAp (Ast.Infix (l, i, r))} ;

%inline list_item:
  | l = lit   {Ast.LConst l}
  | i = IDENT {Ast.LIdent i}

%inline unop:
  | NOT 
    {"!"}
  | MINUS 
    {"-"} ;

%inline binop:
  | AND 
    {"&&"}
  | OR 
    {"||"}
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
  | CONS 
    {"::"}
  | PLUS 
    {"+"}
  | MINUS 
    {"-"}
  | DIV 
    {"/"}
  | MUL 
    {"*"} ;

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
    {Ast.Tuple ts}
  | l = typ ARROW r = typ
    {Ast.Arrow (l, r)} ;
