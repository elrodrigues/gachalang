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

(* PRIMITIVE PARSERS *)
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

(* Bind Combinator *)
(* 'a parser -> ('a -> 'b parser) -> 'b parser *)
(* See test below. Rule is pair <- [0-9]+ "," [0-9]+*)
let parser_bind par callback =
    Parser (fun src ->
        let pr = call_parser par src in
        match pr with
        | None -> None
        | Some (ParseResult (value, new_src)) ->
        call_parser (callback value) new_src
    )

(* BIND TEST: result should be ["12";"34"]
let reg1 = parser_regexp (Str.regexp "[0-9]+")
let reg2 = parser_regexp (Str.regexp ",")
let pair = parser_bind reg1 (fun first -> 
    parser_bind reg2 (fun _ -> 
        parser_bind reg1 (fun second -> parser_constant [first;second])))
call_parser pair Source ("12,34", 0)
*)

(* Complex Parsers *)
(* And Combinator *)
(* 'a parser -> 'b parser -> 'b parser *)
let parser_and parer paree =
    parser_bind parer (fun _ -> paree) 

(* Map Combinator *)
(* 'a parser -> ('a -> 'b) -> 'b parser *)
let parser_map par map =
    parser_bind par (fun value -> parser_constant (map value))

(* AND-MAP TEST: result should be ["12";"34"]
let reg1 = parser_regexp (Str.regexp "[0-9]+")
let reg2 = parser_regexp (Str.regexp ",")
let pair = parser_bind reg1 (fun first -> (parser_and reg2 (parser_map reg1 (fun second -> [first;second]))))
;; call_parser pair (Source ("12,34", 0)) *)

(* Null Combinator *)

(* Maybe Combinator (only for strings)*)
(* If used with bindings, the callback must filter "" *)
(* string parser -> string parser *)
let parser_maybe par =
    parser_or par (parser_constant "")

(* Helper Method *)
(* parseStringToCompletion *)
let parse_string_to_completion par str =
    let src = Source (str, 0) in
    match call_parser par src with
    | None -> raise (GachaError "parse error at index 0")
    | Some (ParseResult (value, new_src)) ->
    match new_src with
    | Source (n_str, index) ->
    match index = (String.length n_str) with
    | false -> raise (GachaError (Printf.sprintf "parse error at index %d" index))
    | true -> value

