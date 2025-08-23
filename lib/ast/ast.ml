open Token_type

type ast =
  | Empty | True | False | Null
  | String of token | Number of token | Id of token
  | Array of ast list
  | ParenExpression of ast
  | Unary of token * ast | Call of ast * ast list list
  | Factor of (token * ast) list
  | Term of (token * ast) list
  | Comparison of (token * ast) list
  | Equality of (token * ast) list
  | LogicAnd of ast list
  | LogicOr of ast list
  | If of ast * ast * ast
  | While of ast * ast
  | Print of ast
  | Block of ast list
  | Declaration of ast * (ast list) * ast
  | Prog of ast list

let string_of_ast ast = 
  let space depth =
    let rec aux res current_depth =
      if current_depth <= 0 then res else aux (res ^ "  ") (current_depth - 1)
    in
    aux "" depth
  in
  let rec aux depth ast = 
    space depth ^ (match ast with
    | Empty -> "Empty" | True -> "True" | False -> "False" | Null -> "Null"
    | String v | Number v | Id v -> Token.string_of_value v.value
    | Array children -> "Array : \n" ^
                        List.fold_left (fun acc child ->
                          acc ^ aux (depth + 1) child
                        ) "" children
    | ParenExpression child -> "ParenExpression : \n" ^ aux (depth + 1) child
    | Unary ( t, child ) -> "Unary : "
                            ^ Token.string_of_value t.value ^ "\n"
                            ^ aux (depth + 1) child
    | Call ( called, arguments_list_list ) -> "Call : \n"
                            ^ aux (depth + 1) called
                            ^ List.fold_left (fun acc arguments_list ->
                                acc ^ ">>" ^
                                (List.fold_left (fun acc2 argument ->
                                  acc2 ^ aux (depth + 1) argument) "" arguments_list)) "" arguments_list_list
    | Factor children -> "Factor : \n" ^ 
                          List.fold_left (fun acc (t, child) ->
                            acc ^ Token.string_of_value t.value ^ aux (depth + 1) child
                          ) "" children
    | Term children -> "Term : \n" ^ 
                          List.fold_left (fun acc (t, child) ->
                            acc ^ Token.string_of_value t.value ^ aux (depth + 1) child
                          ) "" children
    | Comparison children -> "Comparison : \n" ^ 
                          List.fold_left (fun acc (t, child) ->
                            acc ^ Token.string_of_value t.value ^ aux (depth + 1) child
                          ) "" children
    | Equality children -> "Equality : \n" ^ 
                          List.fold_left (fun acc (t, child) ->
                            acc ^ Token.string_of_value t.value ^ aux (depth + 1) child
                          ) "" children
    | LogicAnd children -> "LogicAnd : \n" ^ 
                          List.fold_left (fun acc child ->
                            acc ^ "&&" ^ aux (depth + 1) child
                          ) "" children
    | LogicOr children -> "LogicOr : \n" ^
                          List.fold_left (fun acc child ->
                            acc ^ "||" ^ aux (depth + 1) child
                          ) "" children
    | Block children -> "Block : \n" ^
                        List.fold_left (fun acc child ->
                          acc ^ aux (depth + 1) child
                        ) "" children
    | If ( l_child, m_child, r_child ) -> "If : \n" 
                                          ^ aux (depth + 1) l_child
                                          ^ aux (depth + 1) m_child 
                                          ^ aux (depth + 1) r_child
    | While ( l_child, r_child ) -> "While : \n" 
                                    ^ aux (depth + 1) l_child  
                                    ^ aux (depth + 1) r_child
    | Print child -> "Print : \n" ^ aux (depth + 1) child
    | Declaration ( t, params, child) -> "Declaration  \n" ^ aux (depth + 1) t ^
                                             List.fold_left (fun acc param -> 
                                               acc ^ "\n" ^ aux (depth + 2) param
                                             ) "" params ^ "\n"
                                             ^ aux (depth + 1) child
    | Prog children -> "Prog : \n"
                       ^ List.fold_left (fun acc child -> 
                         acc ^ aux (depth + 1) child
                       ) "" children
    )^ "\n"
  in
  aux 0 ast
