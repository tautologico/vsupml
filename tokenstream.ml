(*

  Very Simple, Untyped, Probabilistic ML
  tokenstream.ml
  Prints the lexer token stream (for debug)

  Andrei de A. Formiga, 2013-02-03

*)

open Parser

let tok2str tok = match tok with
  | IF         -> "IF"
  | THEN       -> "THEN"
  | ELSE       -> "ELSE"
  | LET        -> "LET"
  | IN         -> "IN"
  | INT        -> "INT"
  | PRINTLN    -> "PRINTLN"
  | PLUS       -> "PLUS"
  | MINUS      -> "MINUS"
  | MULT       -> "MULT"
  | DIV        -> "DIV"
  | LT         -> "LT"
  | AND        -> "AND"
  | EQ         -> "EQ"
  | NOT        -> "NOT"
  | SEMICOLON  -> "SEMICOLON"
  | LPAREN     -> "LPAREN"
  | RPAREN     -> "RPAREN"
  | COMMA      -> "COMMA"
  | EOF        -> "EOF"
  | NUM n      -> "NUM " ^ (string_of_int n)
  | ID id      -> "ID " ^ id
  | STR s      -> "STR '" ^ s ^ "'"

let main fname =
  let infile = open_in fname in
  let lexbuf = Lexing.from_channel infile in
  let rec main_loop tok =
    match tok with
      | EOF -> ()
      | _ -> print_endline (tok2str tok); main_loop (Lexer.next_token lexbuf) in
  main_loop (Lexer.next_token lexbuf)

let _ =
  if Array.length Sys.argv < 2 then
    print_endline "Input file required"
  else
    main Sys.argv.(1)
