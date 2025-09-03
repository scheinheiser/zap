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
  | STRING s -> sprintf "STRING %s" s
  | CHAR c -> sprintf "CHAR %c" c
  | BOOL b -> sprintf "BOOL %b" b
  | UNIT -> sprintf "UNIT"
  | TY_PRIM t -> sprintf "TY_PRIM %s" (show_prim t)
  | IDENT i -> sprintf "IDENT %s" i
  | UPPER_IDENT i -> sprintf "UPPER_IDENT %s" i
  | OP o -> sprintf "OP %s" o
  | WHEN -> sprintf "WHEN"
  | WITH -> sprintf "WITH"
  | WITHOUT -> sprintf "WITHOUT"
  | IF -> sprintf "IF"
  | THEN -> sprintf "THEN"
  | ELSE -> sprintf "ELSE"
  | DEC -> sprintf "DEC"
  | DEF -> sprintf "DEF"
  | LET -> sprintf "LET"
  | IN -> sprintf "IN"
  | MODULE -> sprintf "MODULE"
  | IMPORT -> sprintf "IMPORT"
  | AND -> sprintf "AND"
  | NOT -> sprintf "NOT"
  | OR -> sprintf "OR"
  | LT -> sprintf "LT"
  | GT -> sprintf "GT"
  | LTE -> sprintf "LTE"
  | GTE -> sprintf "GTE"
  | NE -> sprintf "NE"
  | PLUS -> sprintf "PLUS"
  | MINUS -> sprintf "MINUS"
  | DIV -> sprintf "DIV"
  | MUL -> sprintf "MUL"
  | CONS -> sprintf "CONS"
  | LBRACE -> sprintf "LBRACE"
  | RBRACE -> sprintf "RBRACE"
  | LPAREN -> sprintf "LPAREN"
  | RPAREN -> sprintf "RPAREN"
  | LBRACK -> sprintf "LBRACK"
  | RBRACK -> sprintf "RBRACK"
  | SEMI -> sprintf "SEMI"
  | SEMISEMI -> sprintf "SEMISEMI"
  | COLON -> sprintf "COLON"
  | EQ -> sprintf "EQ"
  | ASSIGNMENT -> sprintf "ASSIGNMENT"
  | ARROW -> sprintf "ARROW"
  | ATSIGN -> sprintf "ATSIGN"
  | DOT -> sprintf "DOT"
  | COMMA -> sprintf "COMMA"
  | BTICK -> sprintf "BTICK"
  | WILDCARD -> sprintf "WILDCARD"
  | EOF -> sprintf "EOF"
;;

let is_binop (t : token) : bool =
  match t with
  | PLUS | MINUS | MUL | DIV | CONS | NE | EQ | LT | LTE | GT | GTE -> true
  | _ -> false
;;
