open Language
open Parser

let border () =
  Seq.init 30 (fun _ -> '-') |> String.of_seq |> print_endline;
  print_newline ()
;;

let () =
  let _ =
    "def map := go\n\
    \      with\n\
    \        % you can omit the dec for functions/variables in with-blocks\n\
    \        def go _ [] := [];\n\
    \        def bruh f (x :: xs) := f x :: go f xs\n\
    \        ;;\n\
    \    ;;\n\
    \  "
  in
  (* let input = "length (test_lambda 10)" in *)
  let input = In_channel.(open_text "test.zap" |> input_all) in
  print_endline input;
  let l = Lexer.of_string input in
  let res = Parser.parse_program l in
  border ();
  Ast.pp_program Format.std_formatter res
;;
