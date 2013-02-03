/*

  Very Simple, Untyped, Probabilistic ML
  Parser specification for ocamlyacc

  Andrei de A. Formiga, 2013-02-03

*/

%{
  (* open Abs_syntax *)

  let parse_error s = print_endline s
%}

/*** tokens ***/

/* keywords */
%token IF THEN ELSE
%token LET IN
%token INT PRINTLN

/* operators */
%token PLUS MINUS MULT DIV LT AND EQ NOT

/* punctuation */
%token SEMICOLON LPAREN RPAREN COMMA

%token EOF

%token <int> NUM
%token <string> ID
%token <string> STR


%start program
%type <int> program

%%

program: NUM EOF      { 0 }

%%
