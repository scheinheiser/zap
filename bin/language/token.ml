open Util

type token =
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
  | WHEN
  | WITH
  | WITHOUT
  | IF
  | THEN
  | ELSE
  | DEC
  | DEF
  | LET
  | IN
  | MODULE
  | IMPORT
  | AND
  | NOT
  | OR
  | LT
  | GT
  | LTE
  | GTE
  | NE
  | PLUS
  | MINUS
  | DIV
  | MUL
  | CONS
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
  | ATSIGN
  | DOT
  | COMMA
  | BTICK
  | WILDCARD
  | EOF

type t = Location.t * token
