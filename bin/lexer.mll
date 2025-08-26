{
  open Lexing
  open Parser
  open Ast

  let is_generic (i: string) =
    match i.[0] with
    | '\'' -> true
    | _    -> false

  let keywords = [
    ("if", IF);
    ("then", THEN);
    ("else", ELSE);
    ("let", LET);
    ("import", IMPORT);
    ("int", TY_PRIM Ast.PInt);
    ("float", TY_PRIM Ast.PFloat);
    ("char", TY_PRIM Ast.PChar);
    ("string", TY_PRIM Ast.PString);
    ("bool", TY_PRIM Ast.PBool);
    ("()", TY_PRIM Ast.PUnit);
    ("with", WITH);
    ("true", BOOL true);
    ("false", BOOL false);
    ("without", WITHOUT);
    ("when", WHEN);
    ("dec", DEC);
    ("def", DEF);
  ]

  let builtins = []

  exception InvalidChar of string
  exception InvalidEscape of string
}

(*TODO: allow for '_' separators.*)
let int = ['-']? ['0'-'9']+
let float = ['-']? ['0'-'9']+ '.' ['0'-'9']

let symbol = ['-' '+' '*' '\\' '&' '(' ')' '{' '}' '=' '|' '@' '>' '<' '%' '$' '^' '#' '!' ';' ':' '?']
let string_reg = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']+ | symbol+
let ident = ['a'-'z'] ['a'- 'z' 'A'-'Z' '0'-'9' '_']*
let whitespace = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"

rule tokenize = parse
  | whitespace  {tokenize lexbuf}
  | newline     {next_line lexbuf; tokenize lexbuf}
  | int as i    {INT (int_of_string i)}
  | float as f  {FLOAT (float_of_string f)}
  | '\"' string_reg as s '\"' {STRING s}
  | '\''        {tokenize_char lexbuf}
  | "()"        {TY_PRIM }
  | '{'         {LBRACE}
  | '}'         {RBRACE}
  | '('         {LPAREN}
  | ')'         {RPAREN}
  | '['         {LBRACK}
  | ']'         {RBRACK}
  | '%'         {skip_comment lexbuf}
  | "%{"        {skip_multiline_comment lexbuf}
  | "->"        {FUNCTION_ARROW}
  | '.'         {DOT}
  | ':'         {COLON}
  | "::"        {COLONCOLON}
  | ';'         {SEMI}
  | ";;"        {SEMISEMI}
  | '='         {EQUALS}
  | ":="        {DEF_EQUALS}
  | ','         {COMMA}
  | ident as i
    {match (List.assoc_opt i keywords) with
      | (Some t) -> t
      | None when is_generic i -> TY_PRIM @@ Ast.PGeneric i
      | None -> IDENT i}
  | eof         {EOF}
and tokenize_char = parse
  | '\\' {tokenize_control lexbuf}
  | ((['a'-'z' 'A'-'Z' '0'-'9' '_'] | symbol) as c) '\'' {c}
  | _ as c {raise (InvalidChar @@ "Invalid character: '" ^ (Char.escaped c) ^ "'.\n")}
and tokenize_control = parse
  | 'n' '\'' {'\n'}
  | 't' '\'' {'\t'}
  | 'b' '\'' {'\b'}
  | 'r' '\'' {'\r'}
  | '\\' '\'' {'\\'}
  | _ as c {raise (InvalidEscape @@ "Invalid escape sequence: '\\" ^ (Char.escaped c) ^ "'\n")}
and skip_comment = parse
  | '\n' {tokenize lexbuf}
  | _    {skip_comment lexbuf}
and skip_multiline_comment = parse
  | "%}" {tokenize lexbuf}
  | _    {skip_multiline_comment lexbuf}
