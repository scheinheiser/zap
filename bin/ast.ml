type ident = string
type variable = string
type arg = string (* for now. *)
type func = string
type module_name = string
type comment = string
type generic = string
type tname = string

type prim =
  | PInt
  | PFloat
  | PString
  | PChar
  | PBool
  | PAtom
  | PUnit
  | PGeneric of string

type ty =
  | Arrow of ty * ty
  | List of ty
  | Constructor of tname * ty list
  | Tuple of ty list
  | Prim of prim

type const =
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool
  | Atom of ident
  | Unit

type term =
  | TLit      of const
  | TLet      of ident * ty option * term
  | TGrouping of term list
  | TAp       of func_ap
and func_ap =
  | Prefix of func * term list
  | Infix of term * func * term

(* for both, they can have functions and types *)
type import_cond =
  | CWith of ident list
  | CWithout of ident list

type import = module_name * import_cond option

type definition =
  | Dec of func * ty list
  | Def of func * arg list * (term list) option * term list * with_block option
    (* identifer, args, optional when-block, body, optional with-block *)

and with_block = definition list

type top_lvl =
  | TDef of definition
  | TImport of import

type program = module_name * (top_lvl list)
