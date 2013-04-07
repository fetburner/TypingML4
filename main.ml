external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

(* 
 * val read_eval_print_loop : unit -> unit
 * ただのREPL
 * ここも適当
 *)
let rec read_eval_print_loop () =
  print_string "# ";
  flush stdout;
  Parser.toplevel Lexer.token (Lexing.from_channel stdin)
    |> Type.of_Exp
    |> Type.to_string
    |> print_endline;
  read_eval_print_loop ()

let _ = read_eval_print_loop ()
