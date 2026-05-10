open Util
open Primitive

type located_pattern = Location.t * pattern

and pattern =
  | PWild (* _ *)
  | PConst of located_const
  | PCons of located_pattern * located_pattern
  | PCtor of ident * located_pattern list

type typed_expr = located_expr * located_expr
and located_expr = Location.t * expr

and expr =
  | Const of located_const
  | Bop of typed_expr * binop * typed_expr
  | Ap of binder * typed_expr * typed_expr
  | Let of located_pattern * typed_expr * typed_expr
  | Lam of located_pattern * typed_expr
  | Match of typed_expr * (located_pattern * typed_expr option * typed_expr) list
  | TypeLit of prim
  | Binding of ident * typed_expr (* x : T *)
  | Pi of typed_expr * typed_expr

type import_cond =
  | CWith of ident list
  | CWithout of ident list

type located_import = Location.t * import
and import = ident * import_cond option

type located_ty_decl = Location.t * ty_decl
and ty_decl = ident * tdecl_type

and tdecl_type =
  | Alias of located_expr
  | Variant of (ident * located_expr) list
  | Record of (ident * located_expr) list

type located_definition = Location.t * definition

and definition =
  bool
  * ident
  * typed_expr
  * located_pattern list
  * typed_expr option
  * typed_expr

type top_lvl =
  | TDef of located_definition
  | TTyDecl of located_ty_decl
  | TImport of located_import

type program =
  ident * located_import list * located_ty_decl list * located_definition list

(* utils *)
val show_pat : located_pattern -> string

val pp_pattern : Format.formatter -> located_pattern -> unit
val pp_expr : Format.formatter -> located_expr -> unit
val pp_typed_expr : Format.formatter -> typed_expr -> unit
val pp_typed_definition : Format.formatter -> located_definition -> unit
val pp_typed_program : Format.formatter -> program -> unit
