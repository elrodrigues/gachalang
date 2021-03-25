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

let binop_equal pred pair1 pair2 =
    match (pred (fst pair1) (fst pair2)) with
    | true -> pred (snd pair1) (snd pair2)
    | _ -> false

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
    | (Equal (l1, r1), Equal (l2, r2)) -> binop_equal ast_equal (l1,r1) (l2,r2)
    | (NotEqual (l1, r1), NotEqual (l2, r2)) -> binop_equal ast_equal (l1,r1) (l2,r2)
    | (Add (l1, r1), Add (l2, r2)) -> binop_equal ast_equal (l1,r1) (l2,r2)
    | (Subtract (l1, r1), Subtract (l2, r2)) -> binop_equal ast_equal (l1,r1) (l2,r2)
    | (Multiply (l1, r1), Multiply (l2, r2)) -> binop_equal ast_equal (l1,r1) (l2,r2)
    | (Divide (l1, r1), Divide (l2, r2)) -> binop_equal ast_equal (l1,r1) (l2,r2)
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
    (* Block *)
    | (Block stmts1, Block stmts2) -> (
        match (List.length stmts1) = (List.length stmts2) with
        | false -> false
        | true -> map_interrupt ast_equal stmts1 stmts2
    )
    (* If *)
    | (If (cnd1, csq1, alt1), If (cnd2, csq2, alt2)) -> (
        match ast_equal cnd1 cnd2 with
        | false -> false
        | true ->
        match ast_equal csq1 csq2 with
        | false -> false
        | true -> ast_equal alt1 alt2
    )
    (* Function Definition *)
    | (Function (n1, par1, body1), Function (n2, par2, body2)) -> (
        match (compare n1 n2) = 0 with
        | false -> false
        | true ->
        match (List.length par1) = (List.length par2) with
        | false -> false
        | true ->
        match map_interrupt (fun x y -> (compare x y) = 0) par1 par2 with
        | false -> false
        | true -> ast_equal body1 body2
    )
    (* Variable Declaration *)
    | (Var (n1, val1), Var (n2, val2)) -> (
        match (compare n1 n2) = 0 with
        | false -> false
        | true -> ast_equal val1 val2
    )
    (* Assignment *)
    | (Assign (n1, val1), Assign (n2, val2)) -> (
        match (compare n1 n2) = 0 with
        | false -> false
        | true -> ast_equal val1 val2
    )
    (* While *)
    | (While (cond1, body1), While (cond2, body2)) -> (
        match ast_equal cond1 cond2 with
        | false -> false
        | true -> ast_equal body1 body2
    )
    | _ -> false

