(* open Lexing *)
open Language
open Parser

let border () = Seq.init 30 (fun _ -> '-') |> String.of_seq |> print_endline
(* print_newline () *)

let () =
  let _ =
    "@module Syntax\n\
     dec func : int -> int.\n\
     def func num :=\n\
    \  let inc := num + 1 in\n\
    \  num\n\
     ;;"
  in
  let input = "let bing : int -> string = { if true then { bing } else { bap } } in" in
  (* let f = In_channel.(open_text "test.zap" |> input_all) in *)
  print_endline input;
  let l = Lexer.of_string input in
  let res = Parser.parse_term l in
  border ();
  (* Ast.pp_program Format.std_formatter res *)
  Ast.pp_term Format.std_formatter res
;;
