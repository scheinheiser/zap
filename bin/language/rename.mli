open Primitive
open Elab

module AR : sig
  val user_bind : int
  val find_ident : located_expr -> string
  val fresh_alpha : string -> ident
  val rename_program : program -> program
end
