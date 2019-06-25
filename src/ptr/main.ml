(* file main.ml *)
(* implemented by Goktug Saatcioglu *)

open AbstractSyntax
open AbstractDomain
open AbstractInterpreter
open AbstractSyntax
open Printer

let _ =
  let lexbuf = Lexing.from_channel stdin in
    try
      let p' = Parser.prog Lexer.token lexbuf in 
      let p'' = built_abstract_syntax p' in
      let p''' = interpret p'' (initialP ()) in
      let margin = 0 in
      print_labelled_node p''' margin
    with
    | Lexer.Error msg ->
        Printf.fprintf stderr "%s%!" msg
    | Parser.Error ->
        Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)
