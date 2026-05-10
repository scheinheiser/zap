open Util

type binder = int

type ident =
  | Str of string
  | GStr of string * int

type located_const = Location.t * const

and const =
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool
  | Atom of ident
  | Unit
  | Ident of ident
  | Udc of ident (* user defined costructor *)

type prim =
  | PInt
  | PFloat
  | PString
  | PChar
  | PBool
  | PAtom
  | PUnit
  | PUni of int (* A : Type n *)

type binop =
  | IAdd | FAdd
  | IMul | FMul
  | ISub | FSub
  | IDiv | FDiv
  | Less
  | Greater
  | LessE
  | GreaterE
  | Equal
  | NotEq
  | And
  | Or
  | Cons
  | User_op of ident

val show_prim : prim -> string
val get_str : ident -> string
val get_str_combine : ident -> string

val pp_ident : Format.formatter -> ident -> unit
val pp_prim : Format.formatter -> prim -> unit
val pp_const : Format.formatter -> located_const -> unit
val pp_binop : Format.formatter -> binop -> unit
