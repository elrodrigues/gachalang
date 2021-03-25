type ast = 
    (* Primary Types *)
    | Number of int
    | String of string
    (* Names *)
    | Id of string
    (* Prefix/Uniry Operator *)
    | Not of ast
    (* Binary Operators *)
    | Equal of ast * ast
    | NotEqual of ast * ast
    | Add of ast * ast
    | Subtract of ast * ast
    | Multiply of ast * ast
    | Divide of ast * ast
    (* Function Call *)
    | Call of string * (ast list)
    (* Return *)
    | Return of ast
    (* Block *)
    | Block of ast list
    (* If *)
    | If of ast * ast * ast
    (* Function Definition *)
    | Function of string * (string list) * ast
    (* Variable Declaration *)
    | Var of string * ast
    (* Assignment *)
    | Assign of string * ast
    (* While *)
    | While of ast * ast
    
(* ('a -> 'b -> bool) -> 'a list -> 'b list -> bool *)
let rec map_interrupt boolfn list1 list2 =
    match (list1, list2) with
    | (hd1::rst1, hd2::rst2) -> (
        match boolfn hd1 hd2 with
        | false -> false
        | true -> map_interrupt boolfn rst1 rst2
    )
    | _ -> true

(* ast -> ast -> bool *)
let rec ast_equal obj1 obj2 =
    match (obj1, obj2) with
    (* Primary Types *)
    | (Number v1, Number v2) -> (v1 = v2)
    | (String v1, String v2) -> ((compare v1 v2) = 0)
    (* Names *)
    | (Id v1, Id v2) -> ((compare v1 v2) = 0)
    (* Prefix/Uniry Operator *)
    | (Not o1, Not o2) -> ast_equal o1 o2
    (* Binary Operators *)
    | (Equal (l1, r1), Equal (l2, r2)) -> (
        match (ast_equal l1 l2) with
        | true -> ast_equal r1 r2
        | _ -> false
    )
    | (NotEqual (l1, r1), NotEqual (l2, r2)) -> (
        match (ast_equal l1 l2) with
        | true -> ast_equal r1 r2
        | _ -> false
    )
    | (Add (l1, r1), Equal (l2, r2)) -> (
        match (ast_equal l1 l2) with
        | true -> ast_equal r1 r2
        | _ -> false
    )
    | (Subtract (l1, r1), Equal (l2, r2)) -> (
        match (ast_equal l1 l2) with
        | true -> ast_equal r1 r2
        | _ -> false
    )
    | (Multiply (l1, r1), Equal (l2, r2)) -> (
        match (ast_equal l1 l2) with
        | true -> ast_equal r1 r2
        | _ -> false
    )
    | (Divide (l1, r1), Equal (l2, r2)) -> (
        match (ast_equal l1 l2) with
        | true -> ast_equal r1 r2
        | _ -> false
    )
    (* Function Calls *)
    | (Call (c1, args1), Call (c2, args2)) -> (
        match (compare c1 c2) = 0 with
        | false -> false
        | true ->
        match (List.length args1) = (List.length args2) with
        | false -> false
        | true -> map_interrupt ast_equal args1 args2
    )
    (* Return *)
    | (Return o1, Return o2) -> ast_equal o1 o2
    | _ -> false

