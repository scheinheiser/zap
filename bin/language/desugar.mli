open Util
open Primitive

(* desugared syntax *)
type located_pattern = Location.t * pattern

and pattern =
  | PWild (* _ *)
  | PConst of located_const
  | PBop of located_pattern * ident * located_pattern
  | PCtor of ident * located_pattern list
  | PTuple of located_pattern list

type located_expr = Location.t * expr

and expr =
  | Bop of located_expr * binop * located_expr
  | Ap of binder * located_expr * located_expr
  (* we give each function a binder to distinguish between user-defined functions and builtins later on *)
  | Tuple of located_expr list
  | Let of
      located_pattern
      * located_expr option
      * located_expr
      * located_expr (* let p₁ ... pₙ : <optional_ty> = e₁ in e₂ *)
  | Match of located_expr * (located_pattern * located_expr option * located_expr) list
  | Lam of located_pattern * located_expr
    (* `fun x y => x` is desugared to `fun x => fun y => x` *)
  | Const of located_const
  | TypeLit of prim
  | Binding of ident * located_expr (* x : T *)
  | Pi of located_expr * located_expr

type located_ty_decl = Location.t * ty_decl
and ty_decl = ident * tdecl_type

and tdecl_type =
  | Alias of located_expr
  | Variant of located_expr * (ident * located_expr) list
  | Record of ident * located_expr * (ident * located_expr) list

type located_definition = Location.t * definition

and definition =
  | Dec of ident * located_expr
  | Def of ident * located_pattern list * located_expr option * located_expr * with_block
(* identifer, args, optional when-block, body, optional with-block *)

and with_block = located_definition list

type program =
  ident * located_import list * located_ty_decl list * located_definition list

type record_info = ident * ident list

val desugar_expr : Ast.located_expr -> record_info list -> located_expr
val desugar_program : Ast.program -> program
val pp_pattern : Format.formatter -> located_pattern -> unit
val pp_expr : Format.formatter -> located_expr -> unit
val pp_definition : Format.formatter -> located_definition -> unit
val pp_program : Format.formatter -> program -> unit
