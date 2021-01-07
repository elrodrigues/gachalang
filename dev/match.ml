type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
    | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
    | Leaf of 'terminal

let reverse lst = 
    let rec intermed_reverse rv = function
    | [] -> rv
    | hd::rst -> intermed_reverse (hd::rv) rst
    in intermed_reverse [] lst

let subset a b = ((List.length a) < (List.length b))

type awksub_nonterminals =
    | Expr | Term | Lvalue | Incrop | Binop | Num

let rec filter_rules fnd rules query_symbol =
    match rules with
    | [] -> reverse fnd
    | rule::rst ->
        match rule with
        | symbl,rhs -> 
            match (symbl = query_symbol) with
            | false -> filter_rules fnd rst query_symbol
            | true -> let new_fnd = (rhs::fnd) in filter_rules new_fnd rst query_symbol
            
let convert_grammar grammar =
    match grammar with
    | start_symbol,rules -> (start_symbol, (filter_rules [] rules))
    

let append b a = 
    let rec append_left b a =
        match a with
        | [] -> b
        | hd::rst -> let new_b = (hd::b) in append_left new_b rst
    in append_left b (reverse a)

let rec process_tree_stack fnd tree_stack =
    match tree_stack with
    | [] -> reverse fnd
    | node_leaf::rst_tree_stack ->
        match node_leaf with
        | Leaf l -> let new_fnd = (l::fnd) in process_tree_stack new_fnd rst_tree_stack
        | Node (_,node_list) -> let new_tree_stack = (append rst_tree_stack node_list) in
            process_tree_stack fnd new_tree_stack

let parse_tree_leaves tree =
    match tree with
    | Leaf leaf -> [leaf]
    | Node (_,node_list) -> process_tree_stack [] node_list
    
(*let rec exists a = function
    | [] -> false
    | hd::rst -> 
        match (hd = a) with
        | false -> exists a rst
        | _ -> true
        
let rec merge acc = function
    | [] -> acc
    | hd::rst -> merge (append hd acc) rst

        
let rec trace_symbols fnd queue grammar = 
    match queue with
    | [] -> fnd
    | hd::rst ->
        match hd with
        | T _ -> trace_symbols fnd rst grammar
        | N n ->
        match (exists n fnd) with
        | true -> trace_symbols fnd rst grammar
        | false -> 
        let new_fnd = n::fnd and rhs = (grammar n) in
        let new_queue = merge rst rhs in
            trace_symbols new_fnd new_queue grammar*)

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
        [[N Num];
        [N Lvalue];
        [N Incrop; N Lvalue];
        [N Lvalue; N Incrop];
        [T"("; N Expr; T")"]]
    | Lvalue ->
        [[T"$"; N Expr]]
    | Incrop ->
        [[T"++"];
        [T"--"]]
    | Binop ->
        [[T"+"];
        [T"-"]]
    | Num ->
        [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
        [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

(* basic match unit *)
let match_frag rule frag =
    match (rule, frag) with
    | ([],_) -> Some(rule, frag, None)
    | (_,[]) -> None
    | (hd::rst, fr_hd::fr_rst) ->
    match hd with
    | T t -> (
        match (t = fr_hd) with
        | false -> None
        | true -> Some (rst, fr_rst, None) (*skip over call if backtracked*)
    )
    | N n -> Some (rule, frag, Some n) (*continuation needed if backtracked*)

(* match stack handler 
  (rules, frag, node, tick?)
  Guarantees/Assumptions
  = Bottom of stack will always be a (<start symbol>, -1) node.
  = Any other state or irregularity will cause a match fail or
    (very unlikely) unspecified behavior.
  = All newly pushed states on the stack will be marked tick? = true
  = When the state is acted on, its tick? is set to false.
  = tick? behavior is/will be specified in the documentation.
*)
let rec backtrack acc n = function
    | (_, _, [], _) -> (acc, ([], [], [], []))
    | (r::r_rs, f::r_f, diff::r_n, t::r_t) -> (
    match acc with
    | (a_r, a_f, a_n, a_t) ->
    match (diff = n) with
    | true -> 
        backtrack (r::a_r, f::a_f, diff::a_n, t::a_t) n (r_rs, r_f, r_n, r_t)
    | false -> 
    ((reverse a_r, reverse a_f, reverse a_n, reverse a_t), (r::r_rs, f::r_f, diff::r_n, t::r_t))
    )
    | _ -> (acc, ([], [], [], []))
    
let rec unpack id stack n_frg n_nd = function
    | [] -> stack
    | rl::r_rls ->
    match stack with
    | (rls, fs, ns, ts) ->
    unpack id (rl::rls, n_frg::fs, (n_nd,id)::ns, false::ts) n_frg n_nd r_rls

let rec stack_handler gram stack =
    let rec is_done = function
        | ([[]], _::[], [(_,-1)], _::[]) -> true
        | _ -> false
    and strap n n_frg stack = 
        let (acc, b_stack) = backtrack ([],[],[],[]) n stack in
        match b_stack with
        | (rl::rst_rl, f::r_f, diff::r_n, t::r_t) -> (
            match acc with
            | (o_r, o_f, o_n, o_t) ->
            match (subset n_frg f) with
            | false ->
            (append (rl::rst_rl) o_r,
                append (f::r_f) o_f,
                append (diff::r_n) o_n,
                append (t::r_t) o_t)
            | true ->
            (append (rl::rst_rl) o_r,
                append (n_frg::r_f) o_f,
                append (diff::r_n) o_n,
                append (true::r_t) o_t)
        )
        | _ -> ([], [], [], [])
    and leftover n = function
        | (rl::r_rls, f::r_f, diff::r_n, t::r_t) -> (
        match (diff = n) with
        | true -> (rl::r_rls, f::r_f, diff::r_n, t::r_t)
        | false -> leftover diff (r_rls, r_f, r_n, r_t)
        )
        | _ -> ([], [], [], [])
    in
    
    match stack with
    | (rl::r_rls, frg::r_frgs, (n,id)::r_n, t::r_t) -> (
    let ret = match_frag rl frg and dn = is_done stack in
    match (rl, dn) with
    | ([], true) -> (
        match t with
        | true -> Some frg
        | false -> None
    )
    | _ ->
    match ret with
    | None -> (*pop operation*)
    let l_stack = (r_rls, r_frgs, r_n, r_t) in (
        match l_stack with
        | ([], _, _, _) -> None (*no more rules to try*)
        | (_::rr_rls, _::rr_frgs, pr_n::rr_n, pr_t::rr_t) -> (
        match ((n,id) = pr_n) with
        | true -> stack_handler gram l_stack
        | false -> 
        match pr_t with
        | true -> stack_handler gram l_stack
        | false ->
        let ln_stack = leftover pr_n (rr_rls, rr_frgs, rr_n, rr_t) in
        stack_handler gram ln_stack
        )
        | _ -> None (*fail state*)
    )
    | Some (n_rl, n_frg, None) -> (*'push' new operation, Term*)
    let n_stack = (n_rl::r_rls, n_frg::r_frgs, (n,id)::r_n, true::r_t) in
    (
        match (n_rl, n_frg) with
        | ([], _) -> let b_stack = strap (n,id) n_frg (r_rls, r_frgs, r_n, r_t) in (*stack*)
            (
            match b_stack with
            | (_::_, _::_, _::_, _::_) ->
                stack_handler gram b_stack
            | _ -> None (*fail state*)
            )
        | _ -> stack_handler gram n_stack
    )
    | Some (_, _, Some nxt_n) -> (*'push' new operation, Expr*)
    let nxt_rls = reverse (gram nxt_n) in
        match rl with
        | [] -> None
        | _::rst ->
    let nxt_stack = match ((nxt_n, id) = (n, id)) with
        | false -> 
        unpack id (rst::r_rls, frg::r_frgs, (n,id)::r_n, false::r_t) frg nxt_n nxt_rls
        | true -> 
        unpack (id+1) (rst::r_rls, frg::r_frgs, (n,id)::r_n, false::r_t) frg nxt_n nxt_rls
        in
    stack_handler gram nxt_stack
    )
    | _ -> None
    
let make_matcher grampair acceptor frag =
    let (st_sym, gram) = grampair in
    let rev_rls = reverse (gram st_sym) in
    let stack = unpack 0 ([[]],[frag],[(st_sym,-1)],[false]) frag st_sym rev_rls in
    let ret = stack_handler gram stack in
    match ret with
    | None -> None
    | Some r -> acceptor r

let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

(* Tests *)
let all = make_matcher awkish_grammar accept_all
let test1 = ((all ["9";"+";"1"])
    = Some [])
let test2 = ((all ["9";"+";"$";"$";"1";"+"])
    = Some ["+"])
let test3 = ((all ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
                "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
                "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
                "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
                "++"; "+"; "0"])
    = Some [])

