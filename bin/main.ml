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
  let input = "let t : int -> int -> bool = lam x y -> { let x' := x + 1 in let y' := y + 1 in x' = y'}" in
  (* let input = In_channel.(open_text "test.zap" |> input_all) in *)
  print_endline input;
  let l = Lexer.of_string input in
  let res = Parser.parse_term l in
  border ();
  (* Ast.pp_term Format.std_formatter res; *)
  let res' = Typecheck.check_term { var_env = []; func_env = [] } res in
  print_endline "checked program:";
  match res' with
  | Ok (t, _) -> Typed_ast.pp_typed_term Format.std_formatter t
  | Error e -> print_endline (Base.Error.to_string_hum e)
;;
