(* made using https://github.com/contificate/summary as a reference *)
open Util
open Primitive

(* operator associativity, for user-defined operators *)
type assoc =
  | R
  | L

module OM = Map.Make (String)

type operator_map = (int * assoc) OM.t

let fresh_om () : operator_map = OM.empty
let ( >>= ) = Base.Or_error.( >>= )
let ( >>| ) = Base.Or_error.( >>| )
let ( let* ) = Base.Or_error.( >>= )
let ( let@ ) = Base.Or_error.( >>| )

module Lexer : sig
  type t = { mutable tokens : Token.t list }
  type 'a result = 'a Base.Or_error.t

  val make_err : Error.t -> 'a result
  val ok : 'a -> 'a result
  val get : t -> Token.t list
  val of_lexbuf : Lexing.lexbuf -> t
  val of_string : string -> t
  val current : t -> Token.t
  val current_pos : t -> Location.t
  val advance : t -> Token.t
  val peek : t -> Token.t
  val skip : t -> am:int -> unit
  val attempt : t -> (t -> 'a result) -> (t -> 'a result) -> 'a result
  val ( <|> ) : (t -> 'a result) -> (t -> 'a result) -> t -> 'a result
  val matches : t -> Token.token -> bool
  val separated_list : t -> sep:Token.token -> (t -> 'a result) -> 'a list result
  val list_with_end : t -> (Token.token -> bool) -> (t -> 'a result) -> 'a list result
  val consume : t -> Token.token -> string -> unit result
  val consume_any_of : t -> Token.token list -> string -> unit result
  val consume_with_pos : t -> Token.token -> string -> Location.t result
  val consume_any_of_with_pos : t -> Token.token list -> string -> Location.t result
  val consume_with : t -> (Token.token -> 'a option) -> string -> 'a result
  val consume_opt : t -> Token.token -> unit
  val gather_user_precs : t -> (operator_map * t) result
end = struct
  type t = { mutable tokens : Token.t list }
  type 'a result = 'a Base.Or_error.t

  let make_err (e : Error.t) : 'a Base.Or_error.t =
    Base.Or_error.error_string @@ Error.format_err e
  ;;

  let ok (v : 'a) : 'a result = Base.Result.Ok v
  let get { tokens } = tokens

  let of_lexbuf (lexbuf : Lexing.lexbuf) : t =
    let rec aux acc =
      let next = Lexer.token lexbuf in
      match next with
      | _, Token.EOF ->
        let res = List.rev acc in
        { tokens = res }
      | _ -> aux (next :: acc)
    in
    aux []
  ;;

  let of_string (s : string) : t = Lexing.from_string s |> of_lexbuf

  let current (stream : t) : Token.t =
    match stream.tokens with
    | t :: _ -> t
    | [] -> Location.dummy_loc, Token.EOF
  ;;

  let current_pos (stream : t) : Location.t = current stream |> fst

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

  let rec attempt (stream : t) (p1 : t -> 'a result) (p2 : t -> 'a result) : 'a result =
    let stream' = stream.tokens in
    match p1 stream with
    | Result.Ok _ as v -> v
    | Result.Error _ ->
      stream.tokens <- stream';
      p2 stream

  and ( <|> ) l r s = attempt s l r

  let matches (stream : t) (tok : Token.token) : bool =
    let _, next = peek stream in
    next = tok
  ;;

  let separated_list (stream : t) ~(sep : Token.token) (p : t -> 'a result)
    : 'a list result
    =
    let rec aux acc =
      let* v = p stream in
      if matches stream sep
      then (
        ignore (advance stream);
        v :: acc |> aux)
      else Result.Ok (v :: acc)
    in
    aux [] >>| List.rev
  ;;

  let list_with_end (stream : t) (is_end : Token.token -> bool) (p : t -> 'a result)
    : 'a list result
    =
    let rec go l acc =
      let _, next = peek l in
      match next with
      | t when is_end t -> ok @@ List.rev acc
      | _ ->
        (match p l with
         | Error _ as e -> e
         | Ok v -> go l (v :: acc))
    in
    go stream []
  ;;

  let consume (stream : t) (tok : Token.token) (msg : string) : unit result =
    let pos, next = advance stream in
    if next <> tok then make_err (Some pos, msg) else ok ()
  ;;

  let consume_any_of (stream : t) (tok : Token.token list) (msg : string) : unit result =
    let pos, next = advance stream in
    if not @@ List.exists (( = ) next) tok then make_err (Some pos, msg) else ok ()
  ;;

  let consume_with_pos (stream : t) (tok : Token.token) (msg : string) : Location.t result
    =
    let pos, next = advance stream in
    if next <> tok then make_err (Some pos, msg) else ok pos
  ;;

  let consume_any_of_with_pos (stream : t) (tok : Token.token list) (msg : string)
    : Location.t result
    =
    let pos, next = advance stream in
    if not @@ List.exists (( = ) next) tok then make_err (Some pos, msg) else ok pos
  ;;

  let consume_with (stream : t) (f : Token.token -> 'a option) (msg : string) : 'a result =
    let pos, next = advance stream in
    match f next with
    | Some v -> ok v
    | None -> make_err (Some pos, msg)
  ;;

  (* optionally consume a token. has no effect if there isn't a match. *)
  let consume_opt (stream : t) (tok : Token.token) : unit =
    let previous = stream.tokens in
    let _, next = advance stream in
    if next <> tok then stream.tokens <- previous
  ;;

  let rec gather_user_precs ({ tokens } : t) : ((int * assoc) OM.t * t) result =
    let open Token in
    let rec go acc ftokens = function
      | [] -> ok (acc, List.rev ftokens)
      | ((s, ATSIGN) as h) :: rest ->
        let l = { tokens = rest } in
        (match () with
         | _ when matches l RASSOC ->
           skip ~am:1 l;
           let* op, assoc, l' = parse_assoc l R s in
           let acc' = OM.add op assoc acc in
           go acc' ftokens (get l')
         | _ when matches l LASSOC ->
           skip ~am:1 l;
           let* op, assoc, l' = parse_assoc l L s in
           let acc' = OM.add op assoc acc in
           go acc' ftokens (get l')
         | _ -> go acc (h :: ftokens) rest)
      | h :: t -> go acc (h :: ftokens) t
    in
    let@ l, r = go (fresh_om ()) [] tokens in
    l, { tokens = r }

  and parse_assoc (stream : t) (assoc : assoc) (s : Location.t)
    : (string * (int * assoc) * t) result
    =
    let* p =
      consume_with
        stream
        (function
          | INT i -> Some i
          | _ -> None)
        "Expected precedence after assoc keyword."
    in
    if p < 1 || p > 10
    then (
      let loc = Location.combine s (current_pos stream) in
      make_err (Some loc, "Operator precedence must lie within range 1-10."))
    else
      let@ o =
        consume_with
          stream
          (function
            | OP o -> Some o
            | _ -> None)
          "Expected operator after precedence."
      in
      o, (p, assoc), stream
  ;;
end

module Parser = struct
  open Token

  let get_bp (t : Token.token) (om : operator_map) : int =
    match t with
    (* parser fail without this in cases like `f (g x)` *)
    | LPAREN | LBRACK | ARROW -> 1
    | NE | DEQ -> 2
    | AND | OR -> 3
    | LT | GT | LTE | GTE -> 4
    | PLUS | MINUS | FPLUS | FMINUS -> 5
    | MUL | DIV | FMUL | FDIV -> 6
    | CONS -> 7
    | OP op ->
      (match OM.find_opt op om with
       | Some (n, _) -> n
       | None -> 8)
    | UPPER_IDENT _
    | IDENT _
    | DOT_SEP_IDENT _
    | INT _
    | FLOAT _
    | CHAR _
    | STRING _
    | BOOL _
    | UNIT -> 9 (* for function application *)
    | EOF -> -1
    | _ -> 0
  ;;

  (* returns operator precedence, accounting for fixity/associativity *)
  let get_bp_with_fixity (op : string) (om : operator_map) : int =
    match OM.find_opt op om with
    | Some (n, R) -> n - 1
    | Some (n, L) -> n
    | None -> 8
  ;;

  let parse_upper_ident (l : Lexer.t) : ident Lexer.result =
    Lexer.consume_with
      l
      (function
        | UPPER_IDENT i -> Some (Str i)
        | _ -> None)
      "Expected an uppercase identifier."
  ;;

  let parse_lower_ident (l : Lexer.t) : ident Lexer.result =
    Lexer.consume_with
      l
      (function
        | IDENT i -> Some (Str i)
        | _ -> None)
      "Expected a lowercase identifier."
  ;;

  let parse_ident (l : Lexer.t) : ident Lexer.result =
    Lexer.consume_with
      l
      (function
        | IDENT i | UPPER_IDENT i -> Some (Str i)
        | _ -> None)
      "Expected an identifier."
  ;;

  let parse_user_op (l : Lexer.t) : string Lexer.result =
    Lexer.consume_with
      l
      (function
        | OP o -> Some o
        | _ -> None)
      "Expected a user-defined operator."
  ;;

  let rec parse_args (l : Lexer.t) : Ast.located_pattern list Lexer.result =
    Lexer.list_with_end
      l
      (function
        | WILDCARD
        | LBRACK
        | LPAREN
        | IDENT _
        | INT _
        | FLOAT _
        | STRING _
        | CHAR _
        | BOOL _
        | ATSIGN
        | UPPER_IDENT _
        | UNIT -> false
        | _ -> true)
      parse_pattern

  and parse_pattern (l : Lexer.t) : Ast.located_pattern Lexer.result =
    match Lexer.advance l with
    | s, LBRACK ->
      let* contents =
        match Lexer.current l |> snd with
        | RBRACK -> Lexer.ok []
        | _ -> Lexer.separated_list l ~sep:SEMI parse_pattern
      in
      let* e =
        Lexer.consume_with_pos l RBRACK "Expected ']' to end empty list pattern."
      in
      parse_pbop l (Location.combine s e, Ast.PList contents)
    | s, LPAREN ->
      (match Lexer.current l with
       | _, UPPER_IDENT i ->
         Lexer.skip ~am:1 l;
         let* ps = Lexer.list_with_end l (( = ) RPAREN) parse_pattern in
         let* e =
           Lexer.consume_with_pos l RPAREN "Expected ')' to end parenthesised pattern."
         in
         parse_pbop l (Location.combine s e, Ast.PCtor (Str i, ps))
       | _ ->
         let* l' = parse_pattern l in
         (match Lexer.current l with
          | _, RPAREN ->
            Lexer.skip ~am:1 l;
            parse_pbop l l'
          | pos, tok ->
            Lexer.make_err
              (Some pos, Printf.sprintf "Expected ')', but got '%s'." (Token.show tok))))
    | s, WILDCARD -> parse_pbop l (s, Ast.PWild)
    | s, IDENT i -> parse_pbop l (s, Ast.PConst (s, Ident (Str i)))
    | s, UPPER_IDENT i ->
      let* loc, v =
        let open Lexer in
        ((fun l ->
           let@ ((e, _) as v) = parse_pattern l in
           Location.combine s e, [ v ])
         <|> fun _ -> ok (s, []))
          l
      in
      parse_pbop l (loc, Ast.PCtor (Str i, v))
    | s, INT i -> parse_pbop l (s, Ast.PConst (s, Int i))
    | s, FLOAT i -> parse_pbop l (s, Ast.PConst (s, Float i))
    | s, STRING i -> parse_pbop l (s, Ast.PConst (s, String i))
    | s, CHAR i -> parse_pbop l (s, Ast.PConst (s, Char i))
    | s, BOOL i -> parse_pbop l (s, Ast.PConst (s, Bool i))
    | s, UNIT -> parse_pbop l (s, Ast.PConst (s, Unit))
    | pos, tok ->
      Lexer.make_err
        ( Some pos
        , Printf.sprintf
            "Expected definition argument pattern, but got '%s'."
            (Token.show tok) )

  and parse_pbop (l : Lexer.t) ((s, _) as left : Ast.located_pattern)
    : Ast.located_pattern Lexer.result
    =
    match Lexer.peek l with
    | _, OP o ->
      Lexer.skip ~am:1 l;
      let@ ((e, _) as next) = parse_pattern l in
      Location.combine s e, Ast.PBop (left, Str o, next)
    | _, CONS ->
      Lexer.skip ~am:1 l;
      let@ ((e, _) as next) = parse_pattern l in
      Location.combine s e, Ast.PBop (left, Str "::", next)
    | _ -> Lexer.ok left
  ;;

  (* https://www.youtube.com/watch?v=2l1Si4gSb9A *)
  let rec parse_expr (l : Lexer.t) (limit : int) (om : operator_map)
    : Ast.located_expr Lexer.result
    =
    let* ((s, _) as left) = nud l om in
    let rec go lf =
      if Lexer.current l |> snd |> Fun.flip get_bp om > limit
      then (
        let _, op_tok = Lexer.advance l in
        let* res = led l lf (get_bp op_tok om) s op_tok om in
        go res)
      else Lexer.ok lf
    in
    go left

  and nud (l : Lexer.t) (om : operator_map) : Ast.located_expr Lexer.result =
    match Lexer.advance l with
    | s, INT i -> Lexer.ok (s, Ast.Const (s, Int i))
    | s, FLOAT f -> Lexer.ok (s, Ast.Const (s, Float f))
    | s, CHAR c -> Lexer.ok (s, Ast.Const (s, Char c))
    | s, STRING str -> Lexer.ok (s, Ast.Const (s, String str))
    | s, BOOL b -> Lexer.ok (s, Ast.Const (s, Bool b))
    | s, UNIT -> Lexer.ok (s, Ast.Const (s, Unit))
    | s, TY_INT -> Lexer.ok (s, Ast.TypeLit PInt)
    | s, TY_FLOAT -> Lexer.ok (s, Ast.TypeLit PFloat)
    | s, TY_STRING -> Lexer.ok (s, Ast.TypeLit PString)
    | s, TY_CHAR -> Lexer.ok (s, Ast.TypeLit PChar)
    | s, TY_BOOL -> Lexer.ok (s, Ast.TypeLit PBool)
    | s, TY_UNIT -> Lexer.ok (s, Ast.TypeLit PUnit)
    | s, TTYPE ->
      (match Lexer.current l with
       | e, INT i ->
         Lexer.skip ~am:1 l;
         Location.combine s e, Ast.TypeLit (PUni i)
       | _ -> s, Ast.TypeLit (PUni 0) (* x : Type => x : Type 0 *))
      |> Lexer.ok
    | s, IDENT i ->
      let open Lexer in
      ((fun l ->
        let* _ = Lexer.consume l COLON "Expected a ':' between identifier and type in binding." in
        let@ (e, _) as t = parse_expr l 0 om in
        Location.combine s e, Ast.Binding (Str i, t))
      <|> fun _ -> Lexer.ok (s, Ast.Const (s, Ident (Str i))))
        l
    | s, UPPER_IDENT i ->
      let open Lexer in
      ((fun l ->
        let@ e, existing_i, fields = parse_record_update l om in
        Location.combine s e, Ast.RUpdate (Str i, existing_i, fields))
        <|>
      (fun l ->
       let@ e, fields = parse_record_fields l om in
       Location.combine s e, Ast.RCons (Str i, fields))
        <|> 
      (fun l -> 
        let* _ = consume l COLON "Expected a ':' between identifier and type in binding." in
        let@ (e, _) as t = parse_expr l 0 om in
        Location.combine s e, Ast.Binding (Str i, t))
        <|> 
      fun _ -> ok (s, Ast.Const (s, Udc (Str i))))
        l
    | s, DOT_SEP_IDENT is ->
      let is = List.map (fun i -> Str i) is in
      Lexer.ok (s, Ast.Const (s, AccessIdent is))
    | s, LET -> parse_let l om s
    | s, IF -> parse_if l om s
    | s, FUN -> parse_lam l om s
    | s, MATCH -> parse_match l om s
    | s, LBRACK ->
      let* es =
        match Lexer.current l |> snd with
        | RBRACK -> Lexer.ok []
        | _ -> Lexer.separated_list l ~sep:SEMI (fun e -> parse_expr e 0 om)
      in
      let@ e = Lexer.consume_with_pos l RBRACK "Expected ']' to end list." in
      Location.combine s e, Ast.List es
    | s, LPAREN ->
      let open Lexer in
      ((fun l ->
         let* o = parse_user_op l in
         let@ e = Lexer.consume_with_pos l RPAREN "Expected ')' after prefix operator." in
         let loc = Location.combine s e in
         loc, Ast.Const (loc, Ident (Str o)))
       <|> fun l ->
       let* loc, expr = parse_expr l 0 om in
       match Lexer.current l with
       | e, RPAREN ->
         Lexer.skip ~am:1 l;
         Lexer.ok (Location.combine s e, expr)
       | _, COMMA ->
         Lexer.skip ~am:1 l;
         let* es = Lexer.separated_list l ~sep:COMMA (fun l -> parse_expr l 0 om) in
         let@ e =
           Lexer.consume_with_pos l RPAREN "Expected ')' to end tuple expression."
         in
         Location.combine s e, Ast.Tuple ((loc, expr) :: es)
       | pos, tok ->
         Lexer.make_err
           ( Some pos
           , Printf.sprintf
               "Expected either ')' to end grouped expression or ',' to start tuple \
                expression, but got '%s'."
               (Token.show tok) ))
        l
    | pos, tok ->
      Lexer.make_err
        ( Some pos
        , Printf.sprintf "Unexpected token while parsing left: %s" (Token.show tok) )

  and led
        (l : Lexer.t)
        (left : Ast.located_expr)
        (lbp : int)
        (s : Location.t)
        (op : Token.token)
        (om : operator_map)
    : Ast.located_expr Lexer.result
    =
    let@ expr =
      match op with
      | PLUS ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, IAdd, e)
      | FPLUS ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, FAdd, e)
      | MINUS ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, ISub, e)
      | FMINUS ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, FSub, e)
      | MUL ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, IMul, e)
      | FMUL ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, FMul, e)
      | DIV ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, IDiv, e)
      | FDIV ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, FDiv, e)
      | CONS ->
        let@ e = parse_expr l (lbp - 1) om in
        Ast.Bop (left, Cons, e)
      | NE ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, NotEq, e)
      | DEQ ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, Equal, e)
      | LT ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, Less, e)
      | LTE ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, LessE, e)
      | GT ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, Greater, e)
      | GTE ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, GreaterE, e)
      | AND ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, And, e)
      | OR ->
        let@ e = parse_expr l lbp om in
        Ast.Bop (left, Or, e)
      | OP o ->
        let@ e = parse_expr l (get_bp_with_fixity o om) om in
        Ast.Bop (left, User_op (Str o), e)
      | ARROW ->
        let@ e = parse_expr l (lbp - 1) om in
        Ast.Pi (left, e)
      | INT i -> Lexer.ok (Ast.Ap (0, left, (s, Ast.Const (s, Int i))))
      | FLOAT f -> Lexer.ok (Ast.Ap (0, left, (s, Ast.Const (s, Float f))))
      | CHAR c -> Lexer.ok (Ast.Ap (0, left, (s, Ast.Const (s, Char c))))
      | STRING str -> Lexer.ok (Ast.Ap (0, left, (s, Ast.Const (s, String str))))
      | BOOL b -> Lexer.ok (Ast.Ap (0, left, (s, Ast.Const (s, Bool b))))
      | UNIT -> Lexer.ok (Ast.Ap (0, left, (s, Ast.Const (s, Unit))))
      | IDENT i ->
        let open Lexer in
        let* r =
          ((fun l ->
            let* _ = consume l COLON "Expected ':' to separate identifier and type in binding." in
            let@ (e, _) as t = parse_expr l 0 om in
            Location.combine s e, Ast.Binding (Str i, t))
          <|>
          (fun _ ->
            ok (s, Ast.Const (s, Udc (Str i))))) l
        in
        Lexer.ok @@ Ast.Ap (0, left, r)
      | UPPER_IDENT i ->
        let open Lexer in
        let* r =
          ((fun l ->
            let@ e, existing_i, fields = parse_record_update l om in
            Location.combine s e, Ast.RUpdate (Str i, existing_i, fields))
          <|>
          (fun l ->
            let@ e, fields = parse_record_fields l om in
            Location.combine s e, Ast.RCons (Str i, fields))
          <|>
          (fun l ->
            let* _ = Lexer.consume l COLON "Expected ':' to separate identifier and type in binding." in
            let@ (e, _) as t = parse_expr l 0 om in
            Location.combine s e, Ast.Binding (Str i, t))
          <|>
          (fun _ ->
            Lexer.ok (s, Ast.Const (s, Udc (Str i))))) l
        in
        Lexer.ok @@ Ast.Ap (0, left, r)
      | DOT_SEP_IDENT is ->
        let is = List.map (fun i -> Str i) is in
        let r = s, Ast.Const (s, AccessIdent is) in
        Lexer.ok @@ Ast.Ap (0, left, r)
      | TTYPE ->
        (match Lexer.current l with
         | e, INT i -> Ast.Ap (0, left, (Location.combine s e, Ast.TypeLit (PUni i)))
         | _ -> Ast.Ap (0, left, (s, Ast.TypeLit (PUni 0))) (* x : Type => x : Type 0 *))
        |> Lexer.ok
      | LBRACK ->
        let* es = Lexer.separated_list l ~sep:SEMI (fun e -> parse_expr e 0 om) in
        let@ e = Lexer.consume_with_pos l RBRACK "Expected ']' to end list." in
        Ast.Ap (0, left, (Location.combine s e, Ast.List es))
      | LPAREN ->
        let open Lexer in
        let s = Lexer.current_pos l in
        ((fun l ->
           let* o = parse_user_op l in
           let@ e =
             Lexer.consume_with_pos l RPAREN "Expected ')' after prefix operator."
           in
           let loc = Location.combine s e in
           Ast.Ap (0, left, (loc, Ast.Const (loc, Ident (Str o)))))
         <|> fun l ->
         let* loc, expr = parse_expr l 0 om in
         match Lexer.current l with
         | e, RPAREN ->
           Lexer.skip ~am:1 l;
           Lexer.ok @@ Ast.Ap (0, left, (Location.combine s e, expr))
         | _, COMMA ->
           Lexer.skip ~am:1 l;
           let* es = Lexer.separated_list l ~sep:COMMA (fun l -> parse_expr l 0 om) in
           let@ e =
             Lexer.consume_with_pos l RPAREN "Expected ')' to end tuple expression."
           in
           Ast.Ap (0, left, (Location.combine s e, Ast.Tuple ((loc, expr) :: es)))
         | pos, tok ->
           Lexer.make_err
             ( Some pos
             , Printf.sprintf
                 "Expected either ')' to end grouped expression or ',' to start tuple \
                  expression, but got '%s'."
                 (Token.show tok) ))
          l
      | op ->
        Lexer.make_err
          (Some s, Printf.sprintf "Expected binary operator, got '%s'." (Token.show op))
    in
    Location.combine s (Lexer.current_pos l), expr
  [@@ocamlformat "disable"]

  and parse_record_fields (l : Lexer.t) (om : operator_map) : (Location.t * (ident * Ast.located_expr) list) Lexer.result =
    let open Lexer in
    let* _ = consume l LBRACE "Expected a '{' to begin a record constructor." in
    let* fields =
      separated_list l ~sep:SEMI (fun l ->
        let* id = parse_lower_ident l in
        let* _ = consume l EQ "Expected a '=' to separate identifier and expression in record construction." in
        let@ v = parse_expr l 0 om in
        id, v)
    in
    let@ e =
      consume_with_pos l RBRACE "Expected a '}' to end a record constructor."
    in
    e, fields

  and parse_record_update (l : Lexer.t) (om : operator_map) : (Location.t * ident * (ident * Ast.located_expr) list) Lexer.result =
    let* _ = Lexer.consume l LBRACE "Expected '{' to begin record update." in
    let* i = parse_lower_ident l in
    let* _ =
      Lexer.consume l WHERE "Expected 'where' after identifier in record update."
    in
    let* fields =
      Lexer.separated_list l ~sep:SEMI (fun l ->
        let* id = parse_lower_ident l in
        let* _ =
          Lexer.consume
            l
            EQ
            "Expected a '=' to separate identifier and expression in record update."
        in
        let@ v = parse_expr l 0 om in
        id, v)
    in
    let@ e = Lexer.consume_with_pos l RBRACE "Expected '}' to end record update." in
    e, i, fields

  and parse_let (l : Lexer.t) (om : operator_map) (s : Location.t)
    : Ast.located_expr Lexer.result
    =
    let* p = parse_pattern l in
    let* ty =
      match Lexer.current l with
      | _, COLON ->
        Lexer.skip ~am:1 l;
        let* ty = parse_expr l 0 om in
        let@ _ = Lexer.consume l EQ "Expected '=' after type in let-expression." in
        Some ty
      | _, ASSIGNMENT ->
        Lexer.skip ~am:1 l;
        Lexer.ok None
      | pos, tok ->
        Lexer.make_err
          ( Some pos
          , Printf.sprintf "Unexpected token in let-expression: %s" (Token.show tok) )
    in
    let* expr = parse_expr l 0 om in
    let* e = Lexer.consume_with_pos l IN "Expected 'in' to end let-expression." in
    let@ n = parse_expr l 0 om in
    Location.combine s e, Ast.Let (p, ty, expr, n)

  and parse_if (l : Lexer.t) (om : operator_map) (s : Location.t)
    : Ast.located_expr Lexer.result
    =
    let* cond = parse_expr l 0 om in
    let* _ =
      Lexer.consume l THEN "Expected 'then' keyword after if statement condition."
    in
    let* texpr = parse_expr l 0 om in
    let* _ = Lexer.consume l ELSE "Expected 'else' keyword to begin alternate branch." in
    let@ (e, _) as fexpr = parse_expr l 0 om in
    Location.combine s e, Ast.If (cond, texpr, fexpr)

  and parse_lam (l : Lexer.t) (om : operator_map) (s : Location.t)
    : Ast.located_expr Lexer.result
    =
    let* args = parse_args l in
    let* _ = Lexer.consume l ARROW "Expected '->' after lambda arguments." in
    let@ body = parse_expr l 0 om in
    Location.combine s (Lexer.current_pos l), Ast.Lam (args, body)

  and parse_match (l : Lexer.t) (om : operator_map) (s : Location.t)
    : Ast.located_expr Lexer.result
    =
    let* expr = parse_expr l 0 om in
    let* _ = Lexer.consume l TO "Expected 'to' after match subject." in
    let go l =
      let* p = parse_pattern l in
      let* wb =
        match Lexer.current l with
        | _, WHEN ->
          Lexer.skip ~am:1 l;
          let@ e = parse_expr l 0 om in
          Some e
        | _ -> Lexer.ok None
      in
      let* _ = Lexer.consume l F_ARROW "Expected '=>' after match pattern." in
      let@ e = parse_expr l 0 om in
      p, wb, e
    in
    Lexer.consume_opt l PIPE;
    let@ branches = Lexer.separated_list l ~sep:PIPE go in
    Location.combine s (Lexer.current_pos l), Ast.Match (expr, branches)
  ;;

  let rec parse_definition (l : Lexer.t) (om : operator_map)
    : Ast.located_definition Lexer.result
    =
    match Lexer.current l with
    | _, DEC -> parse_dec l om
    | _, DEF -> parse_def l om
    | pos, tok ->
      Lexer.make_err
        ( Some pos
        , Printf.sprintf
            "Expected 'dec' or 'def' keyword to begin a definition, but got '%s'."
            (Token.show tok) )

  and parse_def (l : Lexer.t) (om : operator_map) : Ast.located_definition Lexer.result =
    let* s = Lexer.consume_with_pos l DEF "Expected 'def' keyword." in
    let* n = parse_definition_ident l in
    let* args = parse_args l in
    let* when_block =
      match Lexer.current l with
      | _, COLON ->
        Lexer.skip l ~am:1;
        let* _ = Lexer.consume l WHEN "Expected 'when' after ':' in def." in
        let* block = parse_expr l 0 om in
        let@ _ = Lexer.consume l EQ "Expected '=' after when block." in
        Some block
      | _, ASSIGNMENT ->
        Lexer.skip l ~am:1;
        Lexer.ok None
      | pos, tok ->
        Lexer.make_err
          ( Some pos
          , Printf.sprintf
              "Expected ':' or ':=' after definition arguments, but got '%s'."
              (Token.show tok) )
    in
    let* ((loc, _) as body) = parse_expr l 0 om in
    let@ e, with_block =
      match Lexer.current l with
      | _, WITH ->
        Lexer.skip l ~am:1;
        let rec go l acc last_loc =
          match Lexer.current l with
          | _, DEC | _, DEF ->
            let* ((loc, _) as d) = parse_definition l om in
            go l (d :: acc) loc
          | _ -> Lexer.ok (last_loc, List.rev acc)
        in
        go l [] loc
      | _ -> Lexer.ok (loc, [])
    in
    Location.combine s e, Ast.Def (n, args, when_block, body, with_block)

  and parse_dec (l : Lexer.t) (om : operator_map) : Ast.located_definition Lexer.result =
    let* s = Lexer.consume_with_pos l DEC "Expected 'dec' keyword." in
    let* n = parse_definition_ident l in
    let* _ = Lexer.consume l COLON "Expected ':' after 'dec' keyword." in
    let@ ((e, _) as t) = parse_expr l 0 om in
    Location.combine s e, Ast.Dec (n, t)

  and parse_definition_ident (l : Lexer.t) : ident Lexer.result =
    match Lexer.advance l with
    | _, IDENT i -> Lexer.ok @@ Str i
    | _, LPAREN ->
      let* i = parse_user_op l in
      let@ _ = Lexer.consume l RPAREN "Expected ')' after operator identifier." in
      Str i
    | pos, tok ->
      Lexer.make_err
        ( Some pos
        , Printf.sprintf "Expected identifier or operator, but got '%s'." (Token.show tok)
        )
  ;;

  let rec parse_tydecl (l : Lexer.t) (om : operator_map)
    : Ast.located_ty_decl Lexer.result
    =
    let* s =
      Lexer.consume_with_pos l ATSIGN "Expected '@' before type declaration keyword."
    in
    let@ ident, ty =
      let open Lexer in
      ((fun l -> parse_alias l om)
       <|> (fun l -> parse_union l om)
       <|> fun l -> parse_record l om)
        l
    in
    let e = Lexer.current l |> fst in
    Location.combine s e, (ident, ty)

  and parse_alias (l : Lexer.t) (om : operator_map)
    : (ident * Ast.tdecl_type) Lexer.result
    =
    let* _ = Lexer.consume l ALIAS "Expected alias keyword." in
    let* ident = parse_upper_ident l in
    let* _ = Lexer.consume l ASSIGNMENT "Expected a ':=' for type assignment." in
    let@ t = parse_expr l 0 om in
    ident, Ast.Alias t

  and parse_union (l : Lexer.t) (om : operator_map)
    : (ident * Ast.tdecl_type) Lexer.result
    =
    let* _ = Lexer.consume l UNION "Expected union keyword." in
    let* ident = parse_upper_ident l in
    let* _ = Lexer.consume l COLON "Expected a ':' before type signature." in
    let* tsig = parse_expr l 0 om in
    let* _ = Lexer.consume l EQ "Expected a '=' after type signature." in
    Lexer.consume_opt l PIPE;
    let parse_variant l om =
      let* ident =
        match Lexer.current l with
        | _, LPAREN ->
          Lexer.skip ~am:1 l;
          let* i = parse_user_op l in
          let@ _ = Lexer.consume l RPAREN "Expected a ')' to end operator variant." in
          Str i
        | _ -> parse_upper_ident l
      in
      let* _ = Lexer.consume l COLON "Expected a ':' after variant identifier." in
      let@ e = parse_expr l 0 om in
      ident, e
    in
    let@ variants = Lexer.separated_list l ~sep:PIPE (Fun.flip parse_variant om) in
    ident, Ast.Variant (tsig, variants)

  and parse_record (l : Lexer.t) (om : operator_map)
    : (ident * Ast.tdecl_type) Lexer.result
    =
    let* _ = Lexer.consume l RECORD "Expected record keyword." in
    let* ident = parse_upper_ident l in
    let* _ = Lexer.consume l COLON "Expected a ':' before record type signature." in
    let* t = parse_expr l 0 om in
    let* _ = Lexer.consume l EQ "Expected a '=' after record type signature." in
    let* cons = parse_upper_ident l in
    let parse_field l om =
      let* id = parse_lower_ident l in
      let* _ =
        Lexer.consume l COLON "Expected a ':' to separate the field name and type."
      in
      let@ ty = parse_expr l 0 om in
      id, ty
    in
    let* _ =
      Lexer.consume l LBRACE "Expected a '{' to begin record field declarations."
    in
    let* fields = Lexer.separated_list l ~sep:SEMI (Fun.flip parse_field om) in
    let@ _ = Lexer.consume l RBRACE "Expected a '}' to end record field declarations." in
    ident, Ast.Record (cons, t, fields)
  ;;

  let parse_module (l : Lexer.t) : ident Lexer.result =
    let* _ = Lexer.consume l ATSIGN "Expected an '@' before 'module' keyword." in
    let* _ = Lexer.consume l MODULE "Expected the 'module' keyword." in
    parse_upper_ident l
  ;;

  let rec parse_import (l : Lexer.t) : located_import Lexer.result =
    let* s = Lexer.consume_with_pos l ATSIGN "Expected an '@' before 'import' keyword." in
    let* _ = Lexer.consume l IMPORT "Expected the 'import' keyword." in
    let* name = parse_upper_ident l in
    let@ cond, e =
      if Lexer.matches l WITH || Lexer.matches l WITHOUT
      then
        let@ c, e = parse_import_cond l in
        Some c, e
      else Lexer.ok (None, Lexer.current_pos l)
    in
    Location.combine s e, (name, cond)

  and parse_import_cond (l : Lexer.t) : (import_cond * Location.t) Lexer.result =
    let* cond_type =
      Lexer.consume_with
        l
        (function
          | WITH -> Some true
          | WITHOUT -> Some false
          | _ -> None)
        "Expected an import condition."
    in
    let@ values, e =
      match Lexer.advance l with
      | _, LPAREN ->
        let* contents = Lexer.separated_list l ~sep:COMMA parse_ident in
        let@ p =
          Lexer.consume_with_pos l RPAREN "Expected a ')' to end the import condition."
        in
        contents, p
      | p, IDENT i | p, UPPER_IDENT i -> Lexer.ok ([ Str i ], p)
      | pos, tok ->
        Lexer.make_err
          ( Some pos
          , Printf.sprintf
              "Expected identifier or comma-separated list of identifiers, but got '%s'."
              (Token.show tok) )
    in
    if cond_type then CWith values, e else CWithout values, e
  ;;

  let parse_toplvl (l : Lexer.t) (om : operator_map) : Ast.top_lvl Lexer.result =
    match Lexer.current l with
    | _, ATSIGN ->
      let open Lexer in
      ((fun l ->
         let@ i = parse_import l in
         Ast.TImport i)
      <|>
      (fun l ->
         let@ t = parse_tydecl l om in
         Ast.TTyDecl t))
      l
    | _, DEC ->
      let@ d = parse_dec l om in
      Ast.TDef d
    | _, DEF ->
      let@ d = parse_def l om in
      Ast.TDef d
    | pos, tok ->
      Lexer.make_err
        ( Some pos
        , Printf.sprintf
            "Expected an import, declaration or definition but got '%s'."
            (Token.show tok) )
  ;;

  let parse_program (l : Lexer.t) : Ast.program Lexer.result =
    let* om, l = Lexer.gather_user_precs l in
    let* mod' = parse_module l in
    let@ prog = Lexer.list_with_end l (( = ) EOF) (Fun.flip parse_toplvl om) in
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
