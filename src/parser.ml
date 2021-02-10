open Generated_parser
open Report
open Utils
open Options
open Ast

let parse = parse_S

let pass_parse tokens =
  match parse tokens () with
  | Error msg -> record_compile_result ~error:(Some msg) "Parsing"; Error msg
  | OK (ast, tokens) ->
    record_compile_result "Parsing";
    dump !ast_tree draw_ast_tree ast (call_dot "ast" "AST");
    if !ast_dump then Format.printf "%s\n" (string_of_ast ast) else ();
    OK (ast, tokens)
