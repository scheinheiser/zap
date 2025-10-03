open Language
open Parser
open Rename

let border () = 
  Seq.init 30 (fun _ -> '-') |> String.of_seq |> print_endline;
  print_newline ()

let () =
  let input = In_channel.(open_text "examples/test.zap" |> input_all) in
  (* let input = "length (show [10])" in *)
  let l = Lexer.of_string input in
  let res' = Parser.parse_program l in
  let res = Alpha.rename_program res' in
  Ast.pp_program Format.std_formatter res;
  border ();
  match Typecheck.check_program res with
  | Ok p -> Typed_ast.pp_typed_program Format.std_formatter p
  | Error e -> print_endline (Base.Error.to_string_hum e)
;;
