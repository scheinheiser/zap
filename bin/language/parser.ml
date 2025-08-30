open Util

(* made using https://github.com/contificate/summary and https://www.youtube.com/watch?v=2l1Si4gSb9A as references *)

(* utils *)
let rec any (l : 'a list) (p : 'a -> bool) : bool =
  match l with
  | [] -> false
  | x :: xs -> p x || any xs p
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
  let current ({ tokens } : t) : Token.t = List.hd tokens

  let advance (stream : t) : Token.t =
    match stream.tokens with
    | t :: ts ->
      stream.tokens <- ts;
      t
    | [] -> Location.dummy_loc, Token.EOF
  ;;

  let peek (tokens : t) : Token.t =
    let previous = tokens.tokens in
    let next = advance tokens in
    tokens.tokens <- previous;
    next
  ;;

  let skip (tokens : t) ~am:(n : int) =
    if n < 0
    then
      raise
        (Failure
           "Internal error: called Language.Parser.Lexer.skip with a negative amount.")
    else (
      let rec aux t =
        if t <> 0
        then (
          let _ = advance tokens in
          aux (t - 1))
      in
      aux n)
  ;;

  let matches (tokens : t) (tok : Token.token) : bool =
    let _, next = peek tokens in
    next = tok
  ;;

  let match_separated_list (tokens : t) (sep : Token.token) (p : t -> 'a) : 'a list =
    let rec aux acc =
      let v = p tokens in
      if matches tokens sep
      then (
        let _ = advance tokens in
        v :: acc |> aux)
      else v :: acc
    in
    List.rev @@ aux []
  ;;

  let consume (tokens : t) (tok : Token.token) (msg : string) =
    let pos, next = advance tokens in
    if next <> tok then Error.report_err (Some pos, msg)
  ;;

  let consume_with (tokens : t) (f : Token.token -> 'a option) (msg : string) : 'a =
    let pos, next = advance tokens in
    match f next with
    | Some v -> v
    | None -> Error.report_err (Some pos, msg)
  ;;

  (* optionally consume a token. has no effect if there isn't a match. *)
  let consume_opt (tokens : t) (tok : Token.token) =
    let previous = tokens.tokens in
    let _, next = advance tokens in
    if next <> tok then tokens.tokens <- previous
  ;;

  let consume_any_of (tokens : t) (tok : Token.token list) (msg : string) =
    let pos, next = advance tokens in
    if not @@ any tok (( = ) next) then Error.report_err (Some pos, msg)
  ;;
end

module Parser = struct
  open Token

  let parse_upper_ident (l : Lexer.t) : Ast.ident =
    Lexer.consume_with
      l
      (function
        | UPPER_IDENT i -> Some i
        | _ -> None)
      "Expecting a capitalised identifier."
  ;;

  let parse_ident (l : Lexer.t) : Ast.ident =
    Lexer.consume_with
      l
      (function
        | IDENT i -> Some i
        | _ -> None)
      "Expecting an identifier."
  ;;

  let parse_prim (l : Lexer.t) : Ast.prim =
    Lexer.consume_with
      l
      (function
        | TY_PRIM p -> Some p
        | _ -> None)
      "Expecting a primitive type."
  ;;

  let parse_lit (l : Lexer.t) : Ast.const =
    Lexer.consume_with
      l
      (function
        | INT i -> Some (Ast.Int i)
        | FLOAT f -> Some (Ast.Float f)
        | CHAR c -> Some (Ast.Char c)
        | STRING s -> Some (Ast.String s)
        | BOOL b -> Some (Ast.Bool b)
        | UNIT -> Some Ast.Unit
        | ATSIGN -> Some (Ast.Atom (parse_ident l))
        | _ -> None)
      "Expecting a constant."
  ;;

  let rec parse_ty (l : Lexer.t) : Ast.ty =
    match Lexer.advance l with
    | _, TY_PRIM t when snd (Lexer.peek l) <> ARROW -> Ast.Prim t
    | _, TY_PRIM t ->
      Lexer.skip l ~am:1;
      let next = parse_ty l in
      Ast.Arrow (Ast.Prim t, next)
    | _, LBRACK ->
      let ty = parse_ty l in
      Lexer.consume l RBRACK "Expecting ']' to end list.";
      (match snd (Lexer.peek l) with
       | ARROW ->
         Lexer.skip l ~am:1;
         let next = parse_ty l in
         Ast.Arrow (Ast.List ty, next)
       | _ -> Ast.List ty)
    | _, LPAREN ->
      let contents = Lexer.match_separated_list l COMMA parse_ty in
      Lexer.consume l RPAREN "Expecting ')' to end tuple.";
      (match snd (Lexer.peek l) with
       | ARROW ->
         Lexer.skip l ~am:1;
         let next = parse_ty l in
         Ast.Arrow (Ast.Tuple contents, next)
       | _ -> Ast.Tuple contents)
    | pos, tok ->
      Error.report_err
        ( Some pos
        , Printf.sprintf "Unexpected token while parsing type: %s" (Token.show tok) )
  ;;

  let parse_module (l : Lexer.t) : Ast.module_name =
    Lexer.consume l ATSIGN "Expecting an '@' before 'module' keyword.";
    Lexer.consume l MODULE "Expecting the 'module' keyword.";
    parse_upper_ident l
  ;;

  let parse_import_cond (l : Lexer.t) : Ast.import_cond =
    let cond_type =
      Lexer.consume_with
        l
        (function
          | WITH -> Some true
          | WITHOUT -> Some false
          | _ -> None)
        "Expecting an import condition."
    in
    let values =
      match Lexer.advance l with
      | _, LPAREN ->
        (*TODO: change this to work with uppercase idents too *)
        let contents = Lexer.match_separated_list l COMMA parse_ident in
        Lexer.consume l RPAREN "Expecting a ')' to end the import condition.";
        contents
      | _, IDENT i -> [ i ]
      | _, UPPER_IDENT i -> [ i ]
      | pos, _ ->
        Error.report_err
          (Some pos, "Expected identifier or comma-separated list of identifiers.")
    in
    if cond_type then Ast.CWith values else Ast.CWithout values
  ;;

  let parse_import (l : Lexer.t) : Ast.import =
    Lexer.consume l ATSIGN "Expecting an '@' before 'module' keyword.";
    Lexer.consume l IMPORT "Expecting the 'import' keyword.";
    let name =
      Lexer.consume_with
        l
        (function
          | UPPER_IDENT i -> Some i
          | _ -> None)
        "Expecting a capitalised identifier."
    in
    let cond =
      let _, next = Lexer.peek l in
      if next = WITH || next = WITHOUT then Some (parse_import_cond l) else None
    in
    name, cond
  ;;
end
