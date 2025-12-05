open Util

type t = Location.t * token

and token =
  | INT of int        | TY_INT
  | FLOAT of float    | TY_FLOAT
  | STRING of string  | TY_STRING
  | CHAR of char      | TY_CHAR
  | BOOL of bool      | TY_BOOL
  | UNIT              | TY_UNIT
  | TY_ATOM
  (* | TY_PRIM of Ast.prim *)
  | IDENT of string
  | UPPER_IDENT of string
  | OP of string
  | KOP
  | WHEN
  | WITH
  | MATCH
  | TO
  | WITHOUT
  | RASSOC
  | LASSOC
  | IF
  | THEN
  | ELSE
  | DEC
  | TYPE
  | FORALL
  | TILDE
  | DEF
  | FUN
  | LET
  | IN
  | MODULE
  | IMPORT
  | UNIVERSE
  | TTYPE
  | AND
  | OR
  | LT
  | GT
  | LTE
  | GTE
  | NE
  | PLUS
  | FPLUS
  | MINUS
  | FMINUS
  | DIV
  | FDIV
  | MUL
  | FMUL
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

let show (t : token) : string =
  let open Printf in
  match t with
  | INT i -> sprintf "INT %d" i             | TY_INT -> sprintf "TY_INT"
  | FLOAT f -> sprintf "FLOAT %.5f" f       | TY_FLOAT -> sprintf "TY_FLOAT"
  | STRING s -> sprintf "STRING \"%s\"" s   | TY_STRING -> sprintf "TY_STRING"
  | CHAR c -> sprintf "CHAR %c" c           | TY_CHAR -> sprintf "TY_CHAR"
  | BOOL b -> sprintf "BOOL %b" b           | TY_BOOL -> sprintf "TY_BOOL"
  | UNIT -> "UNIT"                          | TY_UNIT -> "TY_UNIT"
  | TY_ATOM -> "TY_ATOM"
  | IDENT i -> sprintf "IDENT %s" i
  | UPPER_IDENT i -> sprintf "UPPER_IDENT %s" i
  | OP o -> sprintf "OP %s" o
  | KOP -> "KOP"
  | WHEN -> "WHEN"
  | WITH -> "WITH"
  | MATCH -> "MATCH"
  | TO -> "TO"
  | WITHOUT -> "WITHOUT"
  | RASSOC -> "RASSOC"
  | LASSOC -> "LASSOC"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | DEC -> "DEC"
  | TYPE -> "TYPE"
  | FORALL -> "FORALL"
  | TILDE -> "TILDE"
  | DEF -> "DEF"
  | FUN -> "FUN"
  | LET -> "LET"
  | IN -> "IN"
  | MODULE -> "MODULE"
  | IMPORT -> "IMPORT"
  | UNIVERSE -> "UNIVERSE"
  | TTYPE -> "TTYPE"
  | AND -> "AND"
  | OR -> "OR"
  | LT -> "LT"
  | GT -> "GT"
  | LTE -> "LTE"
  | GTE -> "GTE"
  | NE -> "NE"
  | PLUS -> "PLUS"
  | FPLUS -> "FPLUS"
  | MINUS -> "MINUS"
  | FMINUS -> "FMINUS"
  | DIV -> "DIV"
  | FDIV -> "FDIV"
  | MUL -> "MUL"
  | FMUL -> "FMUL"
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
[@@ocamlformat "disable"]

let is_op (t : token) : bool =
  match t with
  | OP _
  | PLUS
  | FPLUS
  | MINUS
  | FMINUS
  | DIV
  | FDIV
  | MUL
  | FMUL
  | AND
  | OR
  | LT
  | GT
  | LTE
  | GTE
  | CONS
  | NE
  | EQ -> true
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
