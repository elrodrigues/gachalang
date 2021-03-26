(* Scanless Parser Combinator *)
(* For regexp functions *)
#load "str.cma"

(* Source type is open to extensions like lazy loading *)
exception GachaError of string
type source = Source of string * int 
type 't parseresult = ParseResult of 't * source
type 'p parser = 
    | Parser of (source -> 'p parseresult option)
    | ErrParser of (source -> 'p)

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
    | ErrParser e -> let ret = e src in 
        Some (ParseResult (ret, Source ("",0)))

(* Regex Combinator *)
(* Str.regexp -> string parser *)
let parser_regexp reg =
    Parser (source_match reg)

(* Constant Combinator *)
(* 'a -> 'a parser *)
let parser_constant value =
    Parser (fun src -> Some (ParseResult (value, src)))

(* Error Combinator *)
(* string -> 'a parser *)
(* #TODO: add line column count from Source *)
let parser_error msg =
    ErrParser (fun src -> raise (GachaError msg))

(* Or Combinator *)
(* 'a parser -> 'a parser -> 'a parser *)
let parser_or parer paree =
    Parser (fun src -> 
        match call_parser parer src with
        | Some pr -> Some pr
        | None -> call_parser paree src
    )

(* ZeroOrMore Combinator *)
(* 'a parser -> 'a list parser *)
let rec parser_zeroOrMore par =
    let rec interm res pars src =
        match call_parser pars src with
        | None -> Some (ParseResult ((List.rev res), src))
        | Some (ParseResult (value, new_src)) -> interm (value::res) pars new_src
    in
    Parser (fun src -> interm [] par src)
