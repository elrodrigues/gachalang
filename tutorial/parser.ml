(* Scanless Parser Combinator *)
(* For regexp functions *)
#load "str.cma"

(* Source type is open to extensions like lazy loading *)
type source = Source of string * int 
type 't parseresult = ParseResult of 't * source
type 'p parser = Parser of (source -> 'p parseresult option)

(* 
FORMAT: class_method
source_match means Class "source" contains Method "match"

Class List: source, parser
*)

(* Str.regexp -> source -> string parseresult option *)
let source_match reg src = 
    match src with
    | Source (str, index) ->
        try (let new_index = (Str.search_forward reg str index) and 
            matched_str = (Str.matched_string str) in 
            let new_src = Source (str, new_index + (String.length matched_str)) in
            Some (ParseResult (matched_str, new_src)))
        with
        | Not_found -> None

(* 'a parser -> source -> 'a parseresult option *)
let call_parser par src = 
    match par with
    | Parser p -> p src

(* Str.regexp -> string parser *)
let parser_regexp reg =
    Parser (source_match reg)

