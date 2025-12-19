open Util

type t = Location.t * token

and token =
  | INT of int
  | TY_INT
  | FLOAT of float
  | TY_FLOAT
  | STRING of string
  | TY_STRING
  | CHAR of char
  | TY_CHAR
  | BOOL of bool
  | TY_BOOL
  | UNIT
  | TY_UNIT
  | TY_ATOM
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

val show : token -> string
val is_op : token -> bool
val op_to_string : token -> string
