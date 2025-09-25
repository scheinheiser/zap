open Util
(* made using https://github.com/contificate/summary as a reference *)

(* utils *)
let is_const (t : Token.token) : bool =
  match t with
  | INT _ | FLOAT _ | STRING _ | CHAR _ | BOOL _ | ATSIGN | UNIT -> true
  | _ -> false
;;

let is_op (t : Token.token) : bool =
  match t with
  | OP _ | PLUS | MINUS | DIV | MUL | AND | OR | LT | GT | LTE | GTE | CONS | NE | EQ ->
    true
  | _ -> false
;;

let op_to_string (t : Token.token) : string =
  match t with
  | OP o -> o
  | PLUS -> "+"
  | MINUS -> "-"
  | DIV -> "/"
  | MUL -> "*"
  | AND -> "&&"
  | OR -> "||"
  | LT -> "<"
  | GT -> ">"
  | LTE -> "<="
  | GTE -> ">="
  | CONS -> "::"
  | NE -> "/="
  | EQ -> "="
  | _ -> ""
;;

let is_delim (t : Token.token) : bool =
  match t with
  | EOF | RPAREN | COMMA | RBRACE | IN | SEMI | SEMISEMI | IF | THEN | ELSE -> true
  | _ -> false
;;

(* main things *)
module Lexer = struct
  type t = { mutable tokens : Token.t list }

  let of_lexbuf (lexbuf : Lexing.lexbuf) : t =
    let rec aux acc =
      let next = Lexer.token lexbuf in
      match next with
      | _, Token.EOF ->
        let res = List.rev (next :: acc) in
        { tokens = res }
      | _ -> aux (next :: acc)
    in
    aux []
  ;;

  let of_string (s : string) : t = Lexing.from_string s |> of_lexbuf
  let current ({ tokens = stream } : t) : Token.t = List.hd stream
  let current_pos ({ tokens = stream } : t) : Location.t = List.hd stream |> fst

  let advance (stream : t) : Token.t =
    match stream.tokens with
    | t :: ts ->
      stream.tokens <- ts;
      t
    | [] -> Location.dummy_loc, Token.EOF
  ;;

  let peek (stream : t) : Token.t =
    let previous = stream.tokens in
    let next = advance stream in
    stream.tokens <- previous;
    next
  ;;

  let skip (stream : t) ~am:(n : int) =
    if n < 0
    then
      raise
        (Error.InternalError "Internal error - called Lexer.skip with a negative step.")
    else (
      let rec aux t =
        if t <> 0
        then (
          let _ = advance stream in
          aux (t - 1))
      in
      aux n)
  ;;

  let matches (stream : t) (tok : Token.token) : bool =
    let _, next = peek stream in
    next = tok
  ;;

  let separated_list (stream : t) (sep : Token.token) (p : t -> 'a) : 'a list =
    let rec aux acc =
      let v = p stream in
      if matches stream sep
      then (
        let _ = advance stream in
        v :: acc |> aux)
      else v :: acc
    in
    List.rev @@ aux []
  ;;

  let list_with_end (stream : t) (is_end : Token.token -> bool) (p : t -> 'a) : 'a list =
    let rec go l acc =
      let _, next = peek l in
      match next with
      | t when is_end t -> List.rev acc
      | _ -> go l (p l :: acc)
    in
    go stream []
  ;;

  let consume (stream : t) (tok : Token.token) (msg : string) =
    let pos, next = advance stream in
    if next <> tok then Error.report_err (Some pos, msg)
  ;;

  let consume_any_of (stream : t) (tok : Token.token list) (msg : string) =
    let pos, next = advance stream in
    if not @@ List.exists (( = ) next) tok then Error.report_err (Some pos, msg) else pos
  ;;

  let consume_with_pos (stream : t) (tok : Token.token) (msg : string) : Location.t =
    let pos, next = advance stream in
    if next <> tok then Error.report_err (Some pos, msg) else pos
  ;;

  let consume_any_of_with_pos (stream : t) (tok : Token.token list) (msg : string)
    : Location.t
    =
    let pos, next = advance stream in
    if not @@ List.exists (( = ) next) tok then Error.report_err (Some pos, msg) else pos
  ;;

  let consume_with (stream : t) (f : Token.token -> 'a option) (msg : string) : 'a =
    let pos, next = advance stream in
    match f next with
    | Some v -> v
    | None -> Error.report_err (Some pos, msg)
  ;;
end

module Parser = struct
  open Token

  let get_bp (t : Token.token) : int =
    match t with
    | LPAREN | RPAREN ->
      1 (* we give this a slight bit of precedence for cases like `length (show 10)` *)
    | NE | EQ -> 2
    | AND | OR -> 3
    | LT | GT | LTE | GTE -> 4
    | PLUS | MINUS -> 5
    | MUL | DIV -> 6
    | NOT -> 7
    | CONS -> 8
    | OP _ -> 9
    | UPPER_IDENT _
    | IDENT _
    | INT _
    | FLOAT _
    | CHAR _
    | STRING _
    | BOOL _
    | UNIT
    | ATSIGN -> 10
    | EOF -> -1
    | _ -> 0
  ;;

  let parse_upper_ident (l : Lexer.t) : Ast.ident =
    Lexer.consume_with
      l
      (function
        | UPPER_IDENT i -> Some i
        | _ -> None)
      "Expected a capitalised identifier."
  ;;

  let parse_ident (l : Lexer.t) : Ast.ident =
    Lexer.consume_with
      l
      (function
        | IDENT i -> Some i
        | _ -> None)
      "Expected an identifier."
  ;;

  let rec parse_ty (l : Lexer.t) : Ast.located_ty =
    match Lexer.advance l with
    | s, TY_PRIM t -> parse_arrow l (s, Ast.Prim t)
    | s, IDENT i -> parse_arrow l (s, Ast.Udt i)
    | s, LBRACK ->
      let ty = parse_ty l in
      let e = Lexer.consume_with_pos l RBRACK "Expected ']' to end list." in
      parse_arrow l (Location.combine s e, Ast.List ty)
    | s, LPAREN ->
      let first = parse_ty l in
      let t =
        match Lexer.current l with
        | _, COMMA ->
          Lexer.skip ~am:1 l;
          let contents = Lexer.separated_list l COMMA parse_ty in
          let e = Lexer.consume_with_pos l RPAREN "Expected ')' to end tuple." in
          parse_arrow l (Location.combine s e, Ast.Tuple (first :: contents))
        | _, ARROW ->
          Lexer.skip ~am:1 l;
          let next = parse_ty l in
          let e =
            Lexer.consume_with_pos l RPAREN "Expected ')' to end parenthesised type."
          in
          Location.combine s e, Ast.Arrow (first, next)
        | _, RPAREN ->
          Lexer.skip ~am:1 l;
          first
        | pos, tok ->
          Error.report_err
            ( Some pos
            , Printf.sprintf "Expected ',', '->' or ')', but got '%s'." (Token.show tok)
            )
      in
      parse_arrow l t
    | pos, tok ->
      Error.report_err
        ( Some pos
        , Printf.sprintf "Unexpected token while parsing type: %s" (Token.show tok) )

  and parse_arrow (l : Lexer.t) (left : Ast.located_ty) =
    match Lexer.peek l with
    | s, ARROW ->
      Lexer.skip l ~am:1;
      let next = parse_ty l in
      Location.combine s (Lexer.current_pos l), Ast.Arrow (left, next)
    | s, IDENT i ->
      Lexer.skip l ~am:1;
      parse_arrow l (Location.combine s (Lexer.current_pos l), Ast.Constructor (i, left))
    | _ -> left
  ;;

  (* https://www.youtube.com/watch?v=2l1Si4gSb9A *)
  let rec parse_expr (l : Lexer.t) (limit : int) : Ast.located_expr =
    let s = Lexer.current_pos l in
    let left = nud l in
    let lbp = Lexer.current l |> snd |> get_bp in
    let rec go lf =
      if lbp > limit && not (Lexer.current l |> snd |> is_delim)
      then (
        let _, op_tok = Lexer.advance l in
        go (led l lf (get_bp op_tok) s op_tok))
      else lf
    in
    go left

  and nud (l : Lexer.t) : Ast.located_expr =
    match Lexer.advance l with
    | s, INT i -> s, Ast.Const (s, Ast.Int i)
    | s, FLOAT f -> s, Ast.Const (s, Ast.Float f)
    | s, CHAR c -> s, Ast.Const (s, Ast.Char c)
    | s, STRING str -> s, Ast.Const (s, Ast.String str)
    | s, BOOL b -> s, Ast.Const (s, Ast.Bool b)
    | s, UNIT -> s, Ast.Const (s, Ast.Unit)
    | s, ATSIGN ->
      let i = parse_ident l
      and loc = Location.combine s (Lexer.current_pos l) in
      loc, Ast.Const (loc, Ast.Atom i)
    | s, IDENT i | s, UPPER_IDENT i -> s, Ast.Ident i
    | s, LBRACK ->
      let es =
        match Lexer.current l |> snd with
        | RBRACK -> []
        | _ -> Lexer.separated_list l SEMI (Fun.flip parse_expr 0)
      in
      let e = Lexer.consume_with_pos l RBRACK "Expected ']' to end list." in
      Location.combine s e, Ast.EList es
    | _, LPAREN ->
      let s = Lexer.current_pos l in
      let loc, e = parse_expr l 0 in
      let expr =
        match Lexer.current l with
        | _, COMMA ->
          Lexer.skip ~am:1 l;
          Ast.ETup ((loc, e) :: Lexer.separated_list l COMMA (Fun.flip parse_expr 0))
        | _, RPAREN -> e
        | pos, tok ->
          Error.report_err
            ( Some pos
            , Printf.sprintf "Expected ')' or ',', but got '%s'." (Token.show tok) )
      in
      let end' =
        Lexer.consume_with_pos l RPAREN "Expected ')' to end grouped expression."
      in
      Location.combine s end', expr
    | s, KOP ->
      let pos, next = Lexer.advance l in
      (match next with
       | o when is_op o -> Location.combine s pos, Ast.Ident (op_to_string o)
       | op ->
         Error.report_err
           ( Some pos
           , Printf.sprintf
               "Expected operator to follow 'op' keyword, but got '%s'."
               (Token.show op) ))
    | pos, tok ->
      Error.report_err
        ( Some pos
        , Printf.sprintf "Unexpected token while parsing left: %s" (Token.show tok) )

  and led
        (l : Lexer.t)
        (left : Ast.located_expr)
        (lbp : int)
        (s : Location.t)
        (op : Token.token)
    : Ast.located_expr
    =
    let expr =
      match op with
      | PLUS -> Ast.Bop (left, Ast.Add, parse_expr l lbp)
      | MINUS -> Ast.Bop (left, Ast.Sub, parse_expr l lbp)
      | MUL -> Ast.Bop (left, Ast.Mul, parse_expr l lbp)
      | DIV -> Ast.Bop (left, Ast.Div, parse_expr l lbp)
      | CONS -> Ast.Bop (left, Ast.Cons, parse_expr l lbp)
      | NE -> Ast.Bop (left, Ast.NotEq, parse_expr l lbp)
      | EQ -> Ast.Bop (left, Ast.Equal, parse_expr l lbp)
      | LT -> Ast.Bop (left, Ast.Less, parse_expr l lbp)
      | LTE -> Ast.Bop (left, Ast.LessE, parse_expr l lbp)
      | GT -> Ast.Bop (left, Ast.Greater, parse_expr l lbp)
      | GTE -> Ast.Bop (left, Ast.GreaterE, parse_expr l lbp)
      | AND -> Ast.Bop (left, Ast.And, parse_expr l lbp)
      | OR -> Ast.Bop (left, Ast.Or, parse_expr l lbp)
      | OP o -> Ast.Bop (left, Ast.User_op o, parse_expr l lbp)
      | INT i -> Ast.Ap (0, left, (s, Ast.Const (s, Ast.Int i)))
      | FLOAT f -> Ast.Ap (0, left, (s, Ast.Const (s, Ast.Float f)))
      | CHAR c -> Ast.Ap (0, left, (s, Ast.Const (s, Ast.Char c)))
      | STRING str -> Ast.Ap (0, left, (s, Ast.Const (s, Ast.String str)))
      | BOOL b -> Ast.Ap (0, left, (s, Ast.Const (s, Ast.Bool b)))
      | UNIT -> Ast.Ap (0, left, (s, Ast.Const (s, Ast.Unit)))
      | IDENT i | UPPER_IDENT i -> Ast.Ap (0, left, (s, Ast.Ident i))
      | ATSIGN ->
        let i = parse_ident l
        and loc = Location.combine s (Lexer.current_pos l) in
        Ast.Ap (0, left, (loc, Ast.Const (loc, Ast.Atom i)))
      | LBRACK ->
        let es = Lexer.separated_list l SEMI (Fun.flip parse_expr 0) in
        let e = Lexer.consume_with_pos l RBRACK "Expected ']' to end list." in
        Ast.Ap (0, left, (Location.combine s e, Ast.EList es))
      | LPAREN ->
        let s = Lexer.current_pos l in
        let loc, e = parse_expr l 0 in
        let expr =
          match Lexer.current l with
          | _, COMMA ->
            Lexer.skip ~am:1 l;
            Ast.ETup ((loc, e) :: Lexer.separated_list l COMMA (Fun.flip parse_expr 0))
          | _, RPAREN -> e
          | pos, tok ->
            Error.report_err
              ( Some pos
              , Printf.sprintf "Expected ')' or ',', but got '%s'." (Token.show tok) )
        in
        let end' =
          Lexer.consume_with_pos l RPAREN "Expected ')' to end grouped expression."
        in
        Ast.Ap (0, left, (Location.combine s end', expr))
      | op ->
        Error.report_err
          (None, Printf.sprintf "Expected binary operator, got '%s'." (Token.show op))
    in
    Location.combine s (Lexer.current_pos l), expr
  ;;

  let rec parse_args (l : Lexer.t) : Ast.located_pattern list =
    Lexer.list_with_end
      l
      (function
        | WILDCARD | LBRACK | LPAREN | IDENT _ -> false
        | t when is_const t -> false
        | _ -> true)
      parse_pattern

  and parse_pattern (l : Lexer.t) : Ast.located_pattern =
    match Lexer.advance l with
    | s, WILDCARD -> s, Ast.PWild
    | s, LBRACK ->
      let contents =
        match Lexer.current l |> snd with
        | RBRACK -> []
        | _ -> Lexer.separated_list l SEMI parse_pattern
      in
      let e = Lexer.consume_with_pos l RBRACK "Expected ']' to end empty list pattern." in
      Location.combine s e, Ast.PList contents
    | s, LPAREN ->
      let l' = parse_pattern l in
      (match Lexer.current l with
       | _, CONS ->
         Lexer.skip ~am:1 l;
         let r = parse_pattern l in
         let e =
           Lexer.consume_with_pos l RPAREN "Expected ')' to end list cons pattern."
         in
         Location.combine s e, Ast.PCons (l', r)
       | _, COMMA ->
         Lexer.skip ~am:1 l;
         let rest = Lexer.separated_list l COMMA parse_pattern in
         let e = Lexer.consume_with_pos l RPAREN "Expected ')' to end tuple pattern." in
         Location.combine s e, Ast.PTup (l' :: rest)
       | _, RPAREN ->
         Lexer.skip ~am:1 l;
         l'
       | pos, tok ->
         Error.report_err
           ( Some pos
           , Printf.sprintf
               "Expected tuple pattern, cons pattern or a pattern, but got '%s'."
               (Token.show tok) ))
    | s, IDENT i -> s, Ast.PIdent i
    | s, INT i -> s, Ast.PConst (s, Ast.Int i)
    | s, FLOAT i -> s, Ast.PConst (s, Ast.Float i)
    | s, STRING i -> s, Ast.PConst (s, Ast.String i)
    | s, CHAR i -> s, Ast.PConst (s, Ast.Char i)
    | s, BOOL i -> s, Ast.PConst (s, Ast.Bool i)
    | s, UNIT -> s, Ast.PConst (s, Ast.Unit)
    | s, ATSIGN ->
      let i = parse_ident l
      and loc = Location.combine s (Lexer.current_pos l) in
      loc, Ast.PConst (loc, Ast.Atom i)
    | pos, tok ->
      Error.report_err
        ( Some pos
        , Printf.sprintf
            "Expected definition argument pattern, but got '%s'."
            (Token.show tok) )
  ;;

  let rec parse_term (l : Lexer.t) : Ast.located_term =
    match Lexer.current l with
    | s, LET ->
      Lexer.skip ~am:1 l;
      let i = parse_ident l in
      let ty =
        match Lexer.current l with
        | _, COLON ->
          Lexer.skip ~am:1 l;
          let ty' = parse_ty l in
          Lexer.consume l EQ "Expected '=' after type in let-expression.";
          Some ty'
        | _, ASSIGNMENT ->
          Lexer.skip ~am:1 l;
          None
        | pos, tok ->
          Error.report_err
            ( Some pos
            , Printf.sprintf "Unexpected token in let-expression: %s" (Token.show tok) )
      in
      let expr = parse_term l in
      Location.combine s (Lexer.current_pos l), Ast.TLet (i, ty, expr)
    | s, LBRACE ->
      Lexer.skip ~am:1 l;
      let terms = Lexer.separated_list l IN parse_term in
      let e = Lexer.consume_with_pos l RBRACE "Expected '}' to end grouping." in
      Location.combine s e, Ast.TGrouping terms
    | s, IF ->
      Lexer.skip ~am:1 l;
      let cond = parse_expr l 0 in
      Lexer.consume l THEN "Expected 'then' keyword after if statement condition.";
      let texpr = parse_term l in
      let fexpr, e =
        match Lexer.current l with
        | _, ELSE ->
          Lexer.skip ~am:1 l;
          Some (parse_term l), Lexer.current_pos l
        | e, _ -> None, e
      in
      Location.combine s e, Ast.TIf (cond, texpr, fexpr)
    | s, LAM ->
      Lexer.skip ~am:1 l;
      let args = parse_args l in
      Lexer.consume l ARROW "Expected '->' after lambda arguments.";
      let body = parse_term l in
      Location.combine s (Lexer.current_pos l), Ast.TLam (args, body)
    | s, _ ->
      let e = Ast.TExpr (parse_expr l 0) in
      Location.combine s (Lexer.current_pos l), e
  ;;

  let rec parse_definition (l : Lexer.t) : Ast.located_definition =
    match Lexer.current l with
    | _, DEC -> parse_dec l
    | _, DEF -> parse_def l
    | pos, tok ->
      Error.report_err
        ( Some pos
        , Printf.sprintf
            "Expected 'dec' or 'def' keyword to begin a definition, but got '%s'."
            (Token.show tok) )

  and parse_def (l : Lexer.t) : Ast.located_definition =
    let s = Lexer.consume_with_pos l DEF "Expected 'def' keyword." in
    let n = parse_ident l in
    let args = parse_args l in
    let when_block =
      match Lexer.current l with
      | _, COLON ->
        Lexer.skip l ~am:1;
        Lexer.consume l WHEN "Expected 'when' after ':' in def.";
        let block = parse_term l in
        Lexer.consume l EQ "Expected '=' after when block.";
        Some block
      | _, ASSIGNMENT ->
        Lexer.skip l ~am:1;
        None
      | pos, tok ->
        Error.report_err
          ( Some pos
          , Printf.sprintf
              "Expected ':' or ':=' after definition arguments, but got '%s'."
              (Token.show tok) )
    in
    let body = Lexer.separated_list l IN parse_term in
    let with_block =
      match Lexer.current l with
      | _, WITH ->
        Lexer.skip l ~am:1;
        let block =
          Lexer.list_with_end l (fun t -> t = SEMI || SEMISEMI = t) parse_definition
        in
        Some block
      | _, SEMISEMI | _, SEMI -> None
      | pos, tok ->
        Error.report_err
          ( Some pos
          , Printf.sprintf
              "Expected either ';;', ';' or with-block to end function definition, but \
               got '%s'."
              (Token.show tok) )
    in
    let e =
      Lexer.consume_any_of_with_pos
        l
        [ SEMISEMI; SEMI ]
        "Expected ';;' or ';' to end function definition."
    in
    Location.combine s e, Ast.Def (n, args, when_block, body, with_block)

  and parse_dec (l : Lexer.t) : Ast.located_definition =
    let s = Lexer.consume_with_pos l DEC "Expected 'dec' keyword." in
    let n = parse_ident l in
    Lexer.consume l COLON "Expected ':' after 'dec' keyword.";
    let t = parse_ty l in
    let e = Lexer.consume_with_pos l DOT "Expected '.' after 'dec' sig." in
    Location.combine s e, Ast.Dec (n, t)
  ;;

  let rec parse_tydecl (l : Lexer.t) : Ast.located_ty_decl =
    let s =
      Lexer.consume_with_pos l TYPE "Expected 'type' keyword before type declaration."
    in
    let ident = parse_ident l in
    Lexer.consume l ASSIGNMENT "Expected ':=' after type identifier.";
    let ty, e = parse_tydecl_type l ident in
    Location.combine s e, (ident, ty)

  and parse_tydecl_type (l : Lexer.t) (ident: Ast.ident) : Ast.tdecl_type * Location.t =
    match Lexer.current l with
    | _, LBRACE ->
      Lexer.skip ~am:1 l;
      let parse_field lex =
        let i = parse_ident lex in
        Lexer.consume l TILDE "Expected '~' after field name.";
        let t = parse_ty lex in
        i, t
      in
      let fields = Lexer.separated_list l SEMI parse_field in
      let e =
        Lexer.consume_with_pos l RBRACE "Expected '}' after field declaration in records."
      in
      Ast.Record fields, e
    | s, PIPE ->
      Lexer.skip ~am:1 l;
      let parse_variant lex =
        let i = parse_upper_ident lex in
        match Lexer.current lex with
        | _, TILDE ->
          Lexer.skip ~am:1 lex;
          let t = parse_ty lex in
          let loc = Location.combine s (Lexer.current_pos l) in
          i, (loc, Ast.Arrow (t, (loc, Ast.Udt ident)))
        | _ -> 
          let loc = Location.combine s (Lexer.current_pos l) in
          i, (loc, Ast.Udt ident)
      in
      let fields = Lexer.separated_list l PIPE parse_variant in
      Ast.Variant fields, Lexer.current_pos l
    | _ ->
      let t = Ast.Alias (parse_ty l) in
      t, Lexer.current_pos l
  ;;

  let parse_module (l : Lexer.t) : Ast.module_name =
    Lexer.consume l ATSIGN "Expected an '@' before 'module' keyword.";
    Lexer.consume l MODULE "Expected the 'module' keyword.";
    parse_upper_ident l
  ;;

  let rec parse_import (l : Lexer.t) : Ast.located_import =
    let s = Lexer.consume_with_pos l ATSIGN "Expected an '@' before 'import' keyword." in
    Lexer.consume l IMPORT "Expected the 'import' keyword.";
    let name = parse_upper_ident l in
    let cond, e =
      if Lexer.matches l WITH || Lexer.matches l WITHOUT
      then (
        let c, e = parse_import_cond l in
        Some c, e)
      else None, Lexer.current_pos l
    in
    Location.combine s e, (name, cond)

  and parse_import_cond (l : Lexer.t) : Ast.import_cond * Location.t =
    let cond_type =
      Lexer.consume_with
        l
        (function
          | WITH -> Some true
          | WITHOUT -> Some false
          | _ -> None)
        "Expected an import condition."
    in
    let values, e =
      match Lexer.advance l with
      | _, LPAREN ->
        let contents = Lexer.separated_list l COMMA parse_import_ident in
        let p =
          Lexer.consume_with_pos l RPAREN "Expected a ')' to end the import condition."
        in
        contents, p
      | p, IDENT i -> [ i ], p
      | p, UPPER_IDENT i -> [ i ], p
      | pos, tok ->
        Error.report_err
          ( Some pos
          , Printf.sprintf
              "Expected identifier or comma-separated list of identifiers, but got '%s'."
              (Token.show tok) )
    in
    if cond_type then Ast.CWith values, e else Ast.CWithout values, e

  and parse_import_ident (l : Lexer.t) : Ast.ident =
    Lexer.consume_with
      l
      (function
        | UPPER_IDENT i -> Some i
        | IDENT i -> Some i
        | _ -> None)
      "Expected an identifier."
  ;;

  let parse_toplvl (l : Lexer.t) : Ast.top_lvl =
    match Lexer.current l with
    | _, ATSIGN -> Ast.TImport (parse_import l)
    | _, DEC -> Ast.TDef (parse_dec l)
    | _, DEF -> Ast.TDef (parse_def l)
    | _, TYPE -> Ast.TTyDecl (parse_tydecl l)
    | pos, tok ->
      Error.report_err
        ( Some pos
        , Printf.sprintf
            "Expected an import, declaration or definition but got '%s'."
            (Token.show tok) )
  ;;

  let parse_program (l : Lexer.t) : Ast.program =
    let mod' = parse_module l in
    let prog = Lexer.list_with_end l (( = ) EOF) parse_toplvl in
    let imports =
      List.filter_map
        (function
          | Ast.TImport i -> Some i
          | _ -> None)
        prog
    in
    let tydecls =
      List.filter_map
        (function
          | Ast.TTyDecl t -> Some t
          | _ -> None)
        prog
    in
    let body =
      List.filter_map
        (function
          | Ast.TDef d -> Some d
          | _ -> None)
        prog
    in
    mod', imports, tydecls, body
  ;;
end
