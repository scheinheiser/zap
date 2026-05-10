module AR : sig
  val user_bind : int
  val find_ident : Ast.located_expr -> string
  val fresh_alpha : string -> Ast.ident
  val rename_program : Ast.program -> Ast.program
end
