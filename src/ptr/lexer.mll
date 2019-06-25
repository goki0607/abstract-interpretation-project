(* File lexer.mll  © P. Cousot 2017 *)
(* changes made by Goktug Saatcioglu to 
    obtain pointer syntax *)
{
open Parser
exception Error of string
}
rule token = parse
    [' ' '\t' '\n'] { token lexbuf } (* skip blanks, tabs and newlines *)
  | "nand"     { NAND }
  | "if"       { IF }
  | "else"     { ELSE }
  | "while"    { WHILE }
  | "break"    { BREAK }
  | "null"     { NULL }
  | (['a'-'z'] | ['A'-'Z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as idt
               { IDENT idt }
  | ['0'-'9']+ as num
               { NUM (int_of_string num) }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '<'        { LT }
  | "=="       { EQ }
  | "!="       { NEQ }
  | '>'        { GT }
  | '='        { ASSIGN }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | ';'        { SEMICOLON }
  | '{'        { LBRACKET }
  | '}'        { RBRACKET }
  | '*'        { DEREF }
  | '&'        { ADDR }
  | eof        { END }
  | _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" 
                                          (Lexing.lexeme_start lexbuf))) }
