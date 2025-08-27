{
  open Lexing
  open Parser

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }

  let is_generic (i: string) =
    match i.[0] with
    | '\'' -> true
    | _    -> false

  let is_upper (s: string) =
    match s.[0] with
    | 'A' .. 'Z' -> true
    | _ -> false

  let keywords = [
    ("if", IF);
    ("then", THEN);
    ("else", ELSE);
    ("let", LET);
    ("in", IN);
    ("not", NOT);
    ("module", MODULE);
    ("import", IMPORT);
    ("int", TY_PRIM Ast.PInt);
    ("float", TY_PRIM Ast.PFloat);
    ("char", TY_PRIM Ast.PChar);
    ("string", TY_PRIM Ast.PString);
    ("bool", TY_PRIM Ast.PBool);
    ("()", TY_PRIM Ast.PUnit);
    ("with", WITH);
    ("without", WITHOUT);
    ("true", BOOL true);
    ("false", BOOL false);
    ("when", WHEN);
    ("dec", DEC);
    ("def", DEF);
  ]

  let builtin_op = [
    ("+", PLUS);
    ("-", MINUS);
    ("*", MUL);
    ("/", DIV);
    ("&&", AND);
    ("||", OR);
    ("!", NOT);
    ("=", EQ);
    ("/=", NE);
    (":=", ASSIGNMENT);
    ("::", CONS);
    ("->", ARROW);
    ("@", ATSIGN);
  ]

  exception InvalidChar of string
  exception InvalidEscape of string
  exception UnterminatedComment of string
}

let int = '-'? ['0'-'9'] ['0'-'9' '_']*
let float = '-'? ['0'-'9']+ '.' ['0'-'9']

let symbol = ['-' '+' '*' '\\' '&' '(' ')' '{' '}' '=' '|' '@' '>' '<' '%' '$' '^' '#' '!' ';' ':' '?']
let op = ['+' '-' '!' '%' '^' '&' '*' '>' '<' '=' '/' '~' '#' '$' '.' '|' '@' ':']

let str = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']+ | symbol+
let ident = ['a'-'z' 'A'-'Z'] ['a'- 'z' 'A'-'Z' '0'-'9' '_']*
let whitespace = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"

rule tokenize = parse
  | whitespace  {tokenize lexbuf}
  | newline     {next_line lexbuf; tokenize lexbuf}
  | int as i    {INT (int_of_string i)}
  | float as f  {FLOAT (float_of_string f)}
  | '\"' str as s '\"' {STRING s}
  | '\''        {tokenize_char lexbuf}
  | '{'         {LBRACE}
  | '}'         {RBRACE}
  | '('         {LPAREN}
  | ')'         {RPAREN}
  | '['         {LBRACK}
  | ']'         {RBRACK}
  | '%'         {skip_comment lexbuf}
  | "%{"        {skip_multiline_comment lexbuf}
  | '.'         {DOT}
  | '`'         {BTICK}
  | ':'         {COLON}
  | ';'         {SEMI}
  | ";;"        {SEMISEMI}
  | ','         {COMMA}
  | op* as op'
    {match (List.assoc_opt op' builtin_op) with
      | (Some op'') -> op''
      | None -> OP op'}
  | ident as i
    {match (List.assoc_opt i keywords) with
      | (Some t) -> t
      | None when is_generic i -> TY_PRIM (Ast.PGeneric i)
      | None when is_upper i   -> UPPER_IDENT i
      | None                   -> IDENT i}
  | eof         {EOF}
and tokenize_char = parse
  | '\\' {tokenize_control lexbuf}
  | ((['a'-'z' 'A'-'Z' '0'-'9' '_'] | symbol) as c) '\'' {CHAR c}
  | _ as c {raise (InvalidChar ("Invalid character: '" ^ (Char.escaped c) ^ "'.\n"))}
and tokenize_control = parse
  | 'n' '\'' {CHAR '\n'}
  | 't' '\'' {CHAR '\t'}
  | 'b' '\'' {CHAR '\b'}
  | 'r' '\'' {CHAR '\r'}
  | '\\' '\'' {CHAR '\\'}
  | _ as c {raise (InvalidEscape ("Invalid escape sequence: '\\" ^ (Char.escaped c) ^ "'\n"))}
and skip_comment = parse
  | '\n' {tokenize lexbuf}
  | _    {skip_comment lexbuf}
  | eof  {raise (UnterminatedComment "Unterminated comment.")}
and skip_multiline_comment = parse
  | "%}" {tokenize lexbuf}
  | _    {skip_multiline_comment lexbuf}
  | eof  {raise (UnterminatedComment "Unterminated comment.")}
