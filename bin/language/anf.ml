open Util

module ANF = struct
  type binder = int
  type ident = string
  type module_name = string

  type value =
    | Int of int
    | String of string
    | Char of char
    | Float of float
    | Bool of bool
    | Atom of string
    | Unit
    | Ident of ident

  type t =
    | Return of value
    | Const of value
    | Tuple of value list
    | Bop of ident * value * Ast.binop * value * t
    | Ap of ident * binder * value * value * t
    | Let of ident * Ast.ty * value * t
    | Grouping of ident * t list
    | If of value * t * t option
    | Join of ident * ident option * t * t
    | Jump of ident * value option

  type definition = ident * Ast.ty * t option * t list (* top level functions *)
  type program = module_name * Ast.ty_decl list * definition list

  let pp_value out (v: value) =
    let open Format in
    match v with
    | Int i -> fprintf out "%d" i
    | String s -> fprintf out "\"%s\"" s
    | Ident i -> fprintf out "%s" i
    | Char c -> fprintf out "'%c'" c
    | Float f -> fprintf out "%.5f" f
    | Bool b -> fprintf out "%b" b
    | Atom a -> fprintf out "@%s" a
    | Unit -> fprintf out "()"

  let rec pp_t out (t: t) =
    match t with
    | Return v -> Format.fprintf out "return %a" pp_value v
    | Const c -> pp_value out c
    | Tuple ts -> Format.fprintf out "(%a)" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") pp_value) ts
    | Bop (i, l, op, r, n)
      -> Format.fprintf out "let %s = %a %a %a in %a" i pp_value l Ast.pp_binop op pp_value r pp_t n
    | Ap (i, _, l, r, n)
      -> Format.fprintf out "let %s = %a %a in %a" i pp_value l pp_value r pp_t n
    | Let (i, t, v, n)
      -> Format.fprintf out "let %s: %a = %a in %a" i Ast.pp_ty (Location.dummy_loc, t) pp_value v pp_t n
    | Grouping (i, vs)
      -> Format.fprintf out "let %s = @[<v>%a@] in" i Format.(pp_print_list ~pp_sep:pp_print_cut pp_t) vs
    | If (cond, t, f)
      -> Format.fprintf out "if@[<v> %a@,then %a@,else %a@]" pp_value cond pp_t t Format.(pp_print_option ~none:(fun out () -> fprintf out "<none>") pp_t) f
    | Jump (i, v) -> Format.fprintf out "jump %s(%a)" i Format.(pp_print_option ~none:(fun out () -> fprintf out "") pp_value) v
    | Join (i, v, b, n)
      -> 
      let pp_string out i = Format.fprintf out "%s" i in
      Format.fprintf out "join@ %s(%a)@ =@ %a in@ %a" i Format.(pp_print_option ~none:(fun out () -> fprintf out "") pp_string) v pp_t b pp_t n

  let pp_definition out ((i, t, wb, body) : definition) =
    Format.fprintf out "let %s: %a (%a) = %a" i Ast.pp_ty (Location.dummy_loc, t) Format.(pp_print_option ~none:(fun out () -> fprintf out "<none>") pp_t) wb Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") pp_t) body

  let pp_program out ((mod_name, decls, defs) : program) =
    let decls = List.map (fun d -> (Location.dummy_loc, d)) decls in
    Format.fprintf out "Module %s@.@.Decls:@.%a@.@.Funcs:@.%a" mod_name Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") Ast.pp_ty_decl) decls Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") pp_definition) defs

  let fresh_temp : unit -> string =
    let i = ref (-1) in
    fun () ->
      incr i;
      "t" ^ string_of_int !i

  let mk_const ((_, c) : Ast.located_const): value = 
    match c with
    | Ast.Int i -> Int i
    | Ast.Float f -> Float f
    | Ast.String s -> String s
    | Ast.Char c -> Char c
    | Ast.Bool b -> Bool b
    | Ast.Unit -> Unit
    | Ast.Atom a -> Atom a
    | Ast.Ident i -> Ident i

  let mk_ret v = Return v

  let rec of_typed_expr (((_, ty), (_, e)): Typed_ast.typed_expr) (f: value -> t): t =
    let open Typed_ast in
    let ( let* ) = (@@) in
    match e with
    | Const c -> f (mk_const c)
    | Bop (l, op, r) ->
      let* l = of_typed_expr l in
      let* r = of_typed_expr r in
      let i = fresh_temp () in
      Bop (i, l, op, r, f (Ident i))
    | Ap (b, l, r) ->
      let* l = of_typed_expr l in
      let* r = of_typed_expr r in
      let i = fresh_temp () in
      Ap (i, b, l, r, f (Ident i))
    | EList l | ETup l -> (* tuples are treated as lists (for now) *)
      let accumulate e ctx values =
        of_typed_expr e (fun v -> ctx (v :: values))
      in
      let base values = 
        let i = fresh_temp () in
        let e = 
          let rec go last_name = function
            | [] -> f (Ident last_name)
            | h :: t ->
              let i = fresh_temp () in
              Bop (i, h, Ast.Cons, Ident last_name, go i t)
          in go i values
        in Let (i, ty, Ident "Nil", e)
      in (List.fold_right accumulate l base) []

  let rec of_typed_term (((_, ty), (_, t)): Typed_ast.typed_term) (f: value -> t): t =
    let open Typed_ast in
    let ( let* ) = (@@) in
    match t with
    | TExpr e -> of_typed_expr e f
    | TLet (i, t) ->
      let* t = of_typed_term t in
      Let (i, ty, t, f (Ident i))
    | TIf (cond, t, f') ->
      let* cond = of_typed_expr cond in
      let i, n = fresh_temp (), fresh_temp () in
      let go value = Jump (i, Some value) in
      (match f' with
      | None -> Join (i, Some n, f (Ident n), If (cond, of_typed_term t go, None))
      | Some f' -> Join (i, Some n, f (Ident n), If (cond, of_typed_term t go, Some (of_typed_term f' go))))
    | TGrouping ts ->
      let i = fresh_temp () in
      Grouping (i, List.map (Fun.flip of_typed_term f) ts)
    | _ -> failwith "todo"

  let rec of_typed_program ((mod_name, _, decls, defs): Typed_ast.program) : program = 
    let decls = List.map (fun (_, t) -> t) decls in
    let defs = List.map of_typed_definition defs in
    (mod_name, decls, defs)
  and of_typed_definition ((_, (i, (_, ty), _, when_block, body)) : Typed_ast.located_definition) : definition =
    let when_block = Base.Option.map when_block ~f:(Fun.flip of_typed_term mk_ret) in
    let body = List.map (Fun.flip of_typed_term mk_ret) body in
    i, ty, when_block, body
end
