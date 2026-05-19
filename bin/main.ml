open! Language
open! Parser
open! Rename

let border () =
  Seq.init 30 (fun _ -> '-') |> String.of_seq |> print_endline;
  print_newline ()
;;

let () =
  (* let input = In_channel.(open_text "examples/normal.zap" |> input_all) in *)
  let input = "{ a : Int }" in
  let l = Lexer.of_string input in
  let res = Parser.parse_expr l 0 (fresh_om ()) in
  match res with
  | Ok res ->
    let res = Desugar.desugar_expr res [] in
    (* let res = AR.rename_program res in *)
    Format.fprintf Format.std_formatter "%a@." Desugar.pp_expr res;
    border ();
    (match (Typecheck.infer (Typecheck.empty ()) res) with
    | Error e -> print_endline (Base.Error.to_string_hum e)
    | Ok (r, _) -> Typed_ast.pp_typed_expr Format.std_formatter r)
  | Error e -> print_endline (Base.Error.to_string_hum e)
;;
