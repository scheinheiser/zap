open Util

type t = Location.t * token

and token =
  | INT of int
  | FLOAT of float
  | STRING of string
  | CHAR of char
  | BOOL of bool
  | UNIT
  | TY_PRIM of Ast.prim
  | IDENT of string
  | UPPER_IDENT of string
  | OP of string
  | KOP
  | WHEN
  | WITH
  | WITHOUT
  | RASSOC
  | LASSOC
  | IF
  | THEN
  | ELSE
  | DEC
  | TYPE
  | TILDE
  | DEF
  | LAM
  | LET
  | IN
  | MODULE
  | IMPORT
  | AND
  | OR
  | LT
  | GT
  | LTE
  | GTE
  | NE
  | PLUS | FPLUS
  | MINUS | FMINUS
  | DIV | FDIV
  | MUL | FMUL
  | CONS
  | PIPE
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | SEMI
  | SEMISEMI
  | COLON
  | EQ
  | ASSIGNMENT
  | ARROW
  | F_ARROW
  | ATSIGN
  | DOT
  | COMMA
  | BTICK
  | WILDCARD
  | EOF

let show_prim = function
  | Ast.PInt -> "int"
  | Ast.PFloat -> "float"
  | Ast.PString -> "string"
  | Ast.PChar -> "char"
  | Ast.PBool -> "bool"
  | Ast.PAtom -> "atom"
  | Ast.PUnit -> "()"
  | Ast.PGeneric n -> n
;;

let show (t : token) : string =
  let open Printf in
  match t with
  | INT i -> sprintf "INT %d" i
  | FLOAT f -> sprintf "FLOAT %.5f" f
  | STRING s -> sprintf "STRING \"%s\"" s
  | CHAR c -> sprintf "CHAR %c" c
  | BOOL b -> sprintf "BOOL %b" b
  | UNIT -> "UNIT"
  | TY_PRIM t -> sprintf "TY_PRIM %s" (show_prim t)
  | IDENT i -> sprintf "IDENT %s" i
  | UPPER_IDENT i -> sprintf "UPPER_IDENT %s" i
  | OP o -> sprintf "OP %s" o
  | KOP -> "KOP"
  | WHEN -> "WHEN"
  | WITH -> "WITH"
  | WITHOUT -> "WITHOUT"
  | RASSOC -> "RASSOC"
  | LASSOC -> "LASSOC"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | DEC -> "DEC"
  | TYPE -> "TYPE"
  | TILDE -> "TILDE"
  | DEF -> "DEF"
  | LAM -> "LAM"
  | LET -> "LET"
  | IN -> "IN"
  | MODULE -> "MODULE"
  | IMPORT -> "IMPORT"
  | AND -> "AND"
  | OR -> "OR"
  | LT -> "LT"
  | GT -> "GT"
  | LTE -> "LTE"
  | GTE -> "GTE"
  | NE -> "NE"
  | PLUS -> "PLUS" | FPLUS -> "FPLUS"
  | MINUS -> "MINUS" | FMINUS -> "FMINUS"
  | DIV -> "DIV" | FDIV -> "FDIV"
  | MUL -> "MUL" | FMUL -> "FMUL"
  | CONS -> "CONS"
  | PIPE -> "PIPE"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | SEMI -> "SEMI"
  | SEMISEMI -> "SEMISEMI"
  | COLON -> "COLON"
  | EQ -> "EQ"
  | ASSIGNMENT -> "ASSIGNMENT"
  | ARROW -> "ARROW"
  | F_ARROW -> "F_ARROW"
  | ATSIGN -> "ATSIGN"
  | DOT -> "DOT"
  | COMMA -> "COMMA"
  | BTICK -> "BTICK"
  | WILDCARD -> "WILDCARD"
  | EOF -> "EOF"
;;

let is_op (t : token) : bool =
  match t with
  | OP _ | PLUS | MINUS | DIV | MUL | AND | OR | LT | GT | LTE | GTE | CONS | NE | EQ ->
    true
  | _ -> false
;;

let op_to_string (t : token) : string =
  match t with
  | OP o -> o
  | PLUS -> "+"
  | MINUS -> "-"
  | DIV -> "/"
  | MUL -> "*"
  | AND -> "&&"
  | OR -> "||"
  | LT -> "<"
  | GT -> ">"
  | LTE -> "<="
  | GTE -> ">="
  | CONS -> "::"
  | NE -> "/="
  | EQ -> "="
  | _ -> ""
;;
