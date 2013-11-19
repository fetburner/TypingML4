(* 
 * ただのREPL
 * ここも適当
 *)
let () =
  while true do
    print_string "# ";
    flush stdout;
    Parser.toplevel Lexer.token (Lexing.from_channel stdin)
    |> Type.of_Exp
    |> Type.to_string
    |> print_endline;
  done
