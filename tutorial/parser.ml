#require "re"
#mod_use "tutorial/ast.ml"
(* Scanless Parser Combinator *)

(* Source type is open to extensions like lazy loading *)
exception GachaError of string
type source = Source of string * int 
type 't parseresult = ParseResult of 't * source
type 'p parser = 
    | Parser of (source -> 'p parseresult option)
    | ErrParser of (source -> 'p)
let a_NULL = Ast.Null

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
        match Re.exec_opt reg str ~pos:index with
        | None -> None
        | Some ret ->
        let _, n_index = Re.Group.offset ret 0 and
        matched_str = Re.Group.get ret 0 in
        Some (ParseResult (matched_str, Source (str, n_index)))

let get_re str =
    Re.Emacs.compile (Re.Emacs.re str)

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
let reg1 = parser_regexp (Re.Emacs.compile (Re.Emacs.re "[0-9]+"))
let reg2 = parser_regexp (Re.Emacs.compile (Re.Emacs.re ","))
let pair = parser_bind reg1 (fun first -> (parser_and reg2 (parser_map reg1 (fun second -> [first;second]))))
;; call_parser pair (Source ("12,34", 0)) *)

(* Maybe Combinator (only for strings)*)
(* If used with bindings, the callback must filter "" *)
(* string parser -> string parser *)
let parser_maybe par =
    parser_or par (parser_constant "")
(* Ast.ast parser -> Ast.ast parser *)
let parser_maybe_ast par =
    parser_or par (parser_constant a_NULL)
(* 'a list parser -> 'a list parser *)
let parser_maybe_lst par = 
    parser_or par (parser_constant [])

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

(* FIRST PASS *)
let whitespace = parser_regexp (get_re "[ \\n\\r\\t]+")
let comments = parser_or (parser_regexp (get_re "//.*$")) (parser_regexp (get_re "/\\*\\(.\\|\n\\)*\\*/"))
let ignored = parser_zeroOrMore (parser_or whitespace comments)
(* Token Parser Combinator *)
let token = (fun reg -> parser_bind (parser_regexp reg) (fun value -> parser_and ignored 
    (parser_constant value)))

(* Token Definitions *)
(* Keywords *)
let t_FUNCTION = token (get_re "function\\b")
let t_IF = token (get_re "if\\b")
let t_ELSE = token (get_re "else\\b")
let t_RETURN = token (get_re "return\\b")
let t_VAR = token (get_re "var\\b")
let t_WHILE = token (get_re "while\\b")
(* Punctuation *)
let t_COMMA = token (get_re "[,]")
let t_SEMICOLON = token (get_re ";")
let t_LEFT_PAREN = token (get_re "[(]")
let t_RIGHT_PAREN = token (get_re "[)]")
let t_LEFT_BRACE = token (get_re "[{]")
let t_RIGHT_BRACE = token (get_re "[}]")

(* AST node generate *)
let ast_NUMBER = parser_map (token (get_re "[0-9]+")) (fun digits -> Ast.Number (int_of_string digits))
let t_ID = token (get_re "[a-zA-Z_][a-zA-Z0-9_]*")
let ast_ID = parser_map t_ID (fun x -> Ast.Id x)
let ast_NOT = parser_map (token (get_re "!")) (fun _ -> Ast.Not a_NULL)
let ast_EQUAL = parser_map (token (get_re "==")) (fun _ -> Ast.Equal (a_NULL,a_NULL))
let ast_EQUAL = parser_map (token (get_re "!=")) (fun _ -> Ast.NotEqual (a_NULL,a_NULL))
let ast_PLUS = parser_map (token (get_re "[+]")) (fun _ -> Ast.Add (a_NULL,a_NULL))
let ast_MINUS = parser_map (token (get_re "[-]")) (fun _ -> Ast.Subtract (a_NULL,a_NULL))
let ast_STAR = parser_map (token (get_re "[*]")) (fun _ -> Ast.Multiply (a_NULL, a_NULL))
let ast_SLASH = parser_map (token (get_re "[/]")) (fun _ -> Ast.Divide (a_NULL,a_NULL))

(* args <- (expression (COMMA expression)\*\)? *)
let args = fun exp -> parser_maybe_lst (parser_and exp 
    (parser_zeroOrMore (parser_and t_COMMA exp)))
(* call <- ID LEFT_PAREN args RIGHT_PAREN *)
let call = fun exp -> parser_bind t_ID (fun callee -> parser_and t_LEFT_PAREN
    (parser_bind (args exp) (fun ags -> parser_and t_RIGHT_BRACE (parser_constant (Ast.Call (callee, ags))))))

