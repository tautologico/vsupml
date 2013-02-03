(*

  Very Simple, Untyped, Probabilistic ML
  Lexer specification for ocamllex

  Andrei de A. Formiga, 2013-02-03

*)

(*** starting code for generating lexer ***)
{
  open Parser

  let remove_quotes s = String.sub s 1 (String.length s - 2)
}

(*** auxiliary definitions ***)
let digit = ['0'-'9']
let letter = ['a'-'z''A'-'Z' '_']

let id = letter (letter | digit)*

let whitespc = [' ' '\r' '\n' '\t']
let linecmt = "//" [^ '\n']*

(*** rules ***)
rule next_token = parse
  digit+ as inum                 { NUM(int_of_string inum) }

| '"' [^'"' '\n']* '"' as s      { STR (remove_quotes s) }

(* keywords *)
| "if"                           { IF        }
| "then"                         { THEN      }
| "else"                         { ELSE      }
| "let"                          { LET       }
| "in"                           { IN        }
| "int"                          { INT       }
| "println"                      { PRINTLN   }

(* operators *)
| '+'                            { PLUS   }
| '-'                            { MINUS  }
| '*'                            { MULT   }
| '/'                            { DIV    }
| '<'                            { LT     }
| "&&"                           { AND    }
| "="                            { EQ     }
| '!'                            { NOT    }

(* punctuation *)
| ';'                            { SEMICOLON }
| '('                            { LPAREN    }
| ')'                            { RPAREN    }
| ','                            { COMMA     }

(* identifiers *)
| id as text                     { ID(text)  }

(* things to ignore *)
| whitespc                       { next_token lexbuf }
| linecmt                        { next_token lexbuf }

| eof                            { EOF }
