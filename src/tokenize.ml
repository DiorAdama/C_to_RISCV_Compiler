open Batteries
open Lexer_generator
open Report
open Utils
open Options
open Symbols

let tokenize file =
  Lexer_generator.tokenize_file file >>= fun tokens ->
  OK (List.map (fun tok -> (tok, None)) tokens)


let pass_tokenize file =
  tokenize file >>* (fun msg ->
      record_compile_result ~error:(Some msg) "Lexing";
      Error msg
    ) $ fun tokens ->
      record_compile_result "Lexing";
      dump !show_tokens (fun oc tokens ->
          List.iter (fun (tok,_) ->
              Format.fprintf oc "%s\n" (string_of_symbol tok)
            ) tokens) tokens (fun f () -> add_to_report "lexer" "Lexer" (Code (file_contents f)));
      OK tokens
