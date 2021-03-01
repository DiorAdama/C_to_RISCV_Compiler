open Batteries
open Symbols
open Utils
(* Expressions régulières *)

(* Nous modélisons les expressions régulières avec le type suivant.

   Une expressions régulière est soit :
   - [Eps] qui dénote l'expressions vide.
   - [Charset cs] dénote l'expression régulière qui matche l'ensemble
     des caractères de l'ensemble [cs].
   - [Cat(r1,r2)] dénote la concaténation de [r1] et [r2] : reconnaît les mots
     [uv] tels que [u] appartient à [r1] et [v] appartient à [r2].
   - [Alt(r1,r2)] dénote un choix entre [r1] et [r2] : reconnaît les mots reconnus
     par [r1] et les mots reconnus par [r2].
   - [Star r] dénote la répétition 0, 1 ou plusieurs fois de l'expression [r].
*)

type 'a set = 'a Set.t

type regexp =
  | Eps
  | Charset of char set
  | Cat of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp

(* [char_regexp c] reconnaît le caractère [c] uniquement. *)
let char_regexp c = Charset (Set.singleton c)

(* [char_range l] reconnaît l'ensemble des caractères de [l]. *)
let char_range (l: char list) =
  Charset (Set.of_list l)

(* [str_regexp s] reconnaît la chaîne de caractère [s]. *)
let str_regexp (s: char list) =
  List.fold_right (fun c reg -> Cat(Charset (Set.singleton c), reg)) s Eps

(* [plus r] reconnaît 1 fois ou plus l'expression [r]. *)
let plus r = Cat(r,Star r)

(* Fonction d'affichage. Peut être utile pour déboguer. *)
let rec string_of_regexp r =
  match r with
    Eps -> "Eps"
  | Charset c -> Printf.sprintf "[%s]" (string_of_char_list (Set.to_list c))
  | Alt (r1,r2) -> Printf.sprintf "(%s)|(%s)"
                     (string_of_regexp r1) (string_of_regexp r2)
  | Cat (r1,r2) -> Printf.sprintf "(%s).(%s)"
                     (string_of_regexp r1) (string_of_regexp r2)
  | Star r -> Printf.sprintf "(%s)*" (string_of_regexp r)


let lowercase_letters = "abcdefghijklmnopqrstuvwxyz"
let uppercase_letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let digits = "0123456789"
let other_characters = "?!=<>_ :;,{}()[]^`-+*/%@\n\t\x00.\"\'\\|~#$&"

(* L'opérateur ^ dénote la concaténation des chaînes de caractères. *)
let alphabet = char_list_of_string (lowercase_letters ^ uppercase_letters ^ digits ^ other_characters)
let letter_regexp = char_range (char_list_of_string (uppercase_letters ^ lowercase_letters))
let digit_regexp = char_range (char_list_of_string digits)
let identifier_material = char_range (char_list_of_string (uppercase_letters ^ lowercase_letters ^ digits ^ "_"))
let keyword_regexp s = str_regexp (char_list_of_string s)

(* La liste des expressions régulières permettant d'identifier les tokens du langage E *)
let list_regexp : (regexp * (string -> token option)) list =
  [
    (keyword_regexp "while",    fun s -> Some (SYM_WHILE));
    (keyword_regexp "int", fun s -> Some (SYM_INT));
    (* begin TODO *)
    (keyword_regexp "void",       fun s -> Some (SYM_VOID));
    (keyword_regexp "char",       fun s -> Some (SYM_CHAR));
    (keyword_regexp "if",       fun s -> Some (SYM_IF));
    (keyword_regexp "else",       fun s -> Some (SYM_ELSE));
    (keyword_regexp "return",       fun s -> Some (SYM_RETURN));
    (keyword_regexp "print",       fun s -> Some (SYM_PRINT));
    (keyword_regexp "struct",       fun s -> Some (SYM_STRUCT));
    (char_regexp '.',       fun s -> Some (SYM_POINT));
    (char_regexp '+',       fun s -> Some (SYM_PLUS));
    (char_regexp '-',       fun s -> Some (SYM_MINUS));
    (char_regexp '*',       fun s -> Some (SYM_ASTERISK));
    (char_regexp '/',       fun s -> Some (SYM_DIV));
    (char_regexp '%',       fun s -> Some (SYM_MOD));
    (char_regexp '[',       fun s -> Some (SYM_LBRACKET));
    (char_regexp ']',       fun s -> Some (SYM_RBRACKET));
    (char_regexp '{',       fun s -> Some (SYM_LBRACE));
    (char_regexp '}',       fun s -> Some (SYM_RBRACE));
    (char_regexp '(',       fun s -> Some (SYM_LPARENTHESIS));
    (char_regexp ')',       fun s -> Some (SYM_RPARENTHESIS));
    (char_regexp ';',       fun s -> Some (SYM_SEMICOLON));
    (char_regexp ',',       fun s -> Some (SYM_COMMA));
    (char_regexp '=',       fun s -> Some (SYM_ASSIGN));
    (keyword_regexp "==",       fun s -> Some (SYM_EQUALITY));
    (keyword_regexp "!=",       fun s -> Some (SYM_NOTEQ));
    (char_regexp '<',       fun s -> Some (SYM_LT));
    (char_regexp '>',       fun s -> Some (SYM_GT));
    (keyword_regexp "<=",       fun s -> Some (SYM_LEQ));
    (keyword_regexp ">=",       fun s -> Some (SYM_GEQ));

    (let id_beginner = char_range (char_list_of_string (uppercase_letters ^ lowercase_letters ^ "_")) in
       Cat (id_beginner, Star identifier_material),       
    fun s -> Some (SYM_IDENTIFIER s));

    (* end TODO *)
    (Cat(keyword_regexp "//",
         Cat(Star (char_range (List.filter (fun c -> c <> '\n') alphabet)),
             Alt (char_regexp '\n', Eps))),
     fun s -> None);
    (Cat(keyword_regexp "/*",
         Cat(
           Star (Alt (
               char_range (List.filter (fun c -> c <> '*') alphabet),
               Cat (Star(char_regexp '*'),
                    plus(char_range (List.filter (fun c -> c <> '/' && c <> '*') alphabet)))
             )),
           Cat(plus (char_regexp '*'), char_regexp '/'))),
     fun s -> None);
    (Cat (char_regexp '\'',
          Cat (char_range (List.filter (fun c -> c <> '\'' && c <> '\\') alphabet),
               char_regexp '\'')),
     fun s ->
       match String.get s 1 with
       | a -> Some (SYM_CHARACTER a)
       | exception Invalid_argument _ -> Some (SYM_CHARACTER 'a')
    );
    (Cat (char_regexp '\'', Cat (char_regexp '\\',
          Cat (char_range (char_list_of_string "\\tn0'"),
               char_regexp '\''))),
     fun s -> match String.get s 2 with
         | '\\' -> Some (SYM_CHARACTER '\\')
         | 'n' -> Some (SYM_CHARACTER '\n')
         | 't' -> Some (SYM_CHARACTER '\t')
         | '\'' -> Some (SYM_CHARACTER '\'')
         | '0' -> Some (SYM_CHARACTER 'a')
         | _ -> None
         | exception _ -> Some (SYM_CHARACTER 'a')
    );
    (Cat (char_regexp '"',
          Cat (Star (
              Alt (
                char_range (List.filter (fun c -> c <> '"' && c <> '\\') alphabet),
                Cat (char_regexp '\\', char_range (char_list_of_string "tn0\\\""))
              )
            ),
               char_regexp '"')),
     fun s -> Some (SYM_STRING (Stdlib.Scanf.unescaped (String.slice ~first:1 ~last:(-1) s))));
    (char_range (char_list_of_string " \t\n"), fun s -> None);
    (plus digit_regexp, fun s -> Some (SYM_INTEGER (int_of_string s)));
    (Eps, fun s -> Some (SYM_EOF))
  ]




  