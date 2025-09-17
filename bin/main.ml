open Language
open Parser

let border () = Seq.init 30 (fun _ -> '-') |> String.of_seq |> print_endline
(* print_newline () *)

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
  let input = "def myfunc := if true then 3 :: [] else [2; 1];;" in
  (* let input = In_channel.(open_text "test.zap" |> input_all) in *)
  print_endline input;
  let l = Lexer.of_string input in
  let res = Parser.parse_definition l in
  border ();
  let res' = Typecheck.check_def { var_env = []; func_env = [] } res in
  print_endline "checked program:";
  match res' with
  | Ok (t, _) -> Format.fprintf Format.std_formatter "%a@." Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") Typed_ast.pp_typed_definition) t
  | Error e -> print_endline (Base.Error.to_string_hum e)
;;
