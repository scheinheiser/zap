(* open Lexing *)
open Language
open Parser

let border () = Seq.init 30 (fun _ -> '-') |> String.of_seq |> print_endline
(* print_newline () *)

let () =
  let input =
    "@module Syntax\n\
     @import Std\n\
     dec func : int -> int.\n\
     %{\n\
     def func num :=\n\
    \  let inc := num + 1 in\n\
    \  num\n\
    ;;\n\
    %}"
  in
  (* let input = "def myfunc _ (k :: ks) : when (p x) = (5, 10) ;;" in *)
  (* let f = In_channel.(open_text "test.zap" |> input_all) in *)
  print_endline input;
  let l = Lexer.of_string input in
  let res = Parser.parse_program l in
  border ();
  Ast.pp_program Format.std_formatter res
  (* Ast.pp_progra Format.std_formatter res *)
;;
