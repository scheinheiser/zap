open Util

type ident = string
type func = string
type module_name = string
type tname = string

type typed_expr = Ast.located_ty * located_expr
and located_expr = Location.t * expr

and expr =
  | Const of Ast.located_const
  | EList of typed_expr list
  | Ident of ident
  | Bop of typed_expr * Ast.binop * typed_expr
  | Ap of typed_expr * typed_expr

type typed_term = Ast.located_ty * located_term
and located_term = Location.t * term

and term =
  | TExpr of typed_expr
  | TLet of ident * typed_term
  | TGrouping of typed_term list
  | TIf of typed_expr * typed_term * typed_term option
  | TTup of typed_expr list
  | TLam of Ast.located_pattern list * typed_term

type located_definition = Location.t * definition

and definition =
  | Dec of func * Ast.located_ty
  | Def of
      func
      * Ast.located_ty list
      * Ast.located_pattern list
      * typed_term option
      * typed_term list
      * with_block option

and with_block = located_definition list

type program =
  module_name
  * Ast.located_import list
  * Ast.located_ty_decl list
  * located_definition list
