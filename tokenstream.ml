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

(** Fold left over the stream of tokens coming from lexbuf. *)
let foldl_map_tokens f lexbuf eofval =
  let rec main_loop tok =
    match tok with
      EOF -> eofval
    | _ -> f tok (main_loop Lexer.next_token lexbuf) in
  main_loop (Lexer.next_token lexbuf)

let dump_tokens fname =
  let infile = open_in fname in
  let lexbuf = Lexing.from_channel infile in
  foldl_tokens (fun tok _ -> print_endline (tok2str tok)) lexbuf ()

(** Tokenize a lexer buffer until EOF and gather tokens in a list. Used by
    other tokenizing functions. *)
let lexbuf_to_list lexbuf =
  foldl_tokens (fun tok rest -> tok :: rest) lexbuf []

(** Tokenize an input file and gather tokens in a list *)
let tokenize_file fname =
  let infile = open_in fname in
  let lexbuf = Lexing.from_channel infile in
  lexbuf_to_list lexbuf

let tokenize_string str =
  let lexbuf = Lexing.from_string str in
  lexbuf_to_list lexbuf


let _ =
  if Array.length Sys.argv < 2 then
    print_endline "Input file required"
  else
    main Sys.argv.(1)
