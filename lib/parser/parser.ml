open Ast
open Token_type

exception Parsing_error of string
exception Try_error

let debug = false

let match_lexeme ( token:token ) ( lexeme_list:lexeme list ) = List.mem token.lexeme lexeme_list

let rec programme res l =
  match l with
  | [] -> 
    begin
    match res with
    | [] -> Empty
    | _ -> Prog (List.rev res)
    end
  | _ -> 
    let (decl_ast, decl_l) = declaration l in
    programme (decl_ast :: res) decl_l

and declaration l =
  let () = if debug then
    (print_endline "Parsing declaration"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  match l with
  | [] -> Empty, l
  | id :: t when match_lexeme id [ID] ->
    begin 
    let rec get_params res l' =
      match l' with
      | [] -> raise (Parsing_error "Expect a declaration but only got an ID and arguments")
      | h :: t when match_lexeme h [ID] -> get_params (res @ [h]) t
      | _ -> res, l'
    in
    let (params_list, params_l) = get_params [] t in
    match params_l with
    | [] -> failwith "Unreachable : declaration"
    | h2 :: t2 when match_lexeme h2 [LEFT_ARROW] ->
      let (decl_ast, decl_l) = declaration t2 in
      Declaration (id, params_list, decl_ast), decl_l
    | _ -> statement l
    end
  | _ -> statement l

and statement l =
  let () = if debug then
    (print_endline "Parsing statement"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  try printStmt l with
  | Try_error ->
    let () = if debug then
      print_endline "printStmt failed, trying whileStmt"
    in
    try whileStmt l with
    | Try_error ->
      let () = if debug then
        print_endline "whileStmt failed, trying block"
      in
      try block l with
      | Try_error ->
        let () = if debug then
          print_endline "block failed, trying ifStmt"
        in
        try ifStmt l with
        | Try_error ->
          let () = if debug then
            print_endline "ifStmt failed, trying expression"
          in
          try expression l with
            | Try_error -> 
              let () = if debug then
                print_endline "expression failed, returning empty"
              in  
              Empty, l

and ifStmt l =
  let () = if debug then
    (print_endline "Parsing if statement"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let expr_ast, expr_l = expression l in
  match expr_l with
  | [] -> raise Try_error
  | h :: t when match_lexeme h [QUESTION_MARK] ->
    begin
    let stmt_true_ast, stmt_true_l = statement t in
    match stmt_true_l with
    | [] -> raise ( Parsing_error "Unfinished if statement" )
    | h2 :: t2 when match_lexeme h2 [COLON] ->
      let stmt_false_ast, stmt_false_l = statement t2 in
      If (expr_ast, stmt_true_ast, stmt_false_ast), stmt_false_l
    | _ -> raise ( Parsing_error "Missing false consequence in if statement" )
    end
  | _ -> raise Try_error

and whileStmt l =
  let () = if debug then
    (print_endline "Parsing while statement"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let stmt_ast, stmt_l = try block l with
  | Try_error ->
    let () = if debug then
      print_endline "block failed, trying expression"
    in
    expression l
  in
  match stmt_l with
  | [] -> raise Try_error
  | h :: t when match_lexeme h [TIME_TIME] ->
    let expr_ast, expr_l = expression t in
    While (expr_ast, stmt_ast),expr_l 
  | _ -> raise Try_error

and printStmt l =
  let () = if debug then
    (print_endline "Parsing print statement"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  match l with
  | [] -> Empty, l
  | h :: t when match_lexeme h [DOLLAR] ->
    let expr_ast, expr_l = expression t in
    Print expr_ast, expr_l
  | _ -> raise Try_error

and block l =
  let () = if debug then
    (print_endline "Parsing block statement"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  match l with
  | [] -> Empty, l
  | h :: t when match_lexeme h [LEFT_BRACE] ->
    let rec decl_star res aux_l =
      match aux_l with
      | [] -> raise (Parsing_error "Unclosed block at end of file")
      | h :: t when match_lexeme h [RIGHT_BRACE] ->
        Block (List.rev res), t
      | _ ->
        let decl_ast, decl_l = declaration aux_l in
        decl_star (decl_ast :: res) decl_l
    in
    decl_star [] t
  | _ -> raise Try_error

and expression l =
  let () = if debug then
    (print_endline "Parsing expression"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  logic_or l

and logic_or l =
  let () = if debug then
    (print_endline "Parsing logic or"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let logic_and_ast, logic_and_l = logic_and l in
  let rec aux res aux_l =
    match aux_l with
    | h :: t when match_lexeme h [D_VERT_BAR] ->
      let logic_and_ast, logic_and_l = logic_and t in
      aux (logic_and_ast :: res) logic_and_l
    | _ -> 
      begin
      match res with
      | [x] -> x, aux_l
      | _ -> LogicOr (List.rev res), aux_l
      end
  in
  aux [logic_and_ast] logic_and_l

and logic_and l =
  let () = if debug then
    (print_endline "Parsing logic_and"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let equality_ast, equality_l = equality l in
  let rec aux res aux_l =
    match aux_l with
    | h :: t when match_lexeme h [D_AMPERSAND] ->
      let equality_ast, equality_l = equality t in
      aux (equality_ast :: res) equality_l
    | _ -> 
      begin
      match res with
      | [x] -> x, aux_l
      | _ -> LogicAnd (List.rev res), aux_l
      end
  in
  aux [equality_ast] equality_l

and equality l =
  let () = if debug then
    (print_endline "Parsing equality"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let comparison_ast, comparison_l = comparison l in
  let rec aux res aux_l =
    match aux_l with
    | h :: t when match_lexeme h [BANG_EQUAL; EQUAL] ->
      let comparison_ast, comparison_l = comparison t in
      aux ((h, comparison_ast ) :: res) comparison_l
    | _ -> 
      begin
      match res with
      | [(_, x)] -> x, aux_l
      | _ -> Equality (List.rev res), aux_l
      end
  in
  aux [({lexeme = EMPTY; value = STRING ""; line = -1}, comparison_ast)] comparison_l

and comparison l =
  let () = if debug then
    (print_endline "Parsing comparison"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let term_ast, term_l = term l in
  let rec aux res aux_l = 
    match aux_l with
    | h :: t when match_lexeme h [SUP; SUP_EQUAL; INF; INF_EQUAL] ->
      let term_ast , term_l = term t in
      aux ((h, term_ast ) :: res) term_l 
    | _ -> 
      begin
      match res with
      | [(_, x)] -> x, aux_l
      | _ -> Comparison (List.rev res), aux_l
      end
  in
  aux [({lexeme = EMPTY; value = STRING ""; line = -1}, term_ast)] term_l 

and term l =
  let () = if debug then
    (print_endline "Parsing term"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let factor_ast, factor_l = factor l in
  let rec aux res aux_l = 
    match aux_l with
    | h :: t when match_lexeme h [MINUS; PLUS] ->
      let factor_ast , factor_l = factor t in
      aux ((h, factor_ast ) :: res) factor_l 
    | _ -> 
      begin
      match res with
      | [(_, x)] -> x, aux_l
      | _ -> Term (List.rev res), aux_l
      end
  in
  aux [({lexeme = EMPTY; value = STRING ""; line = -1}, factor_ast)] factor_l 

and factor l =
  let () = if debug then
    (print_endline "Parsing factor"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let unary_ast, unary_l = unary l in
  let rec aux res aux_l = 
    match aux_l with
    | h :: t when match_lexeme h [SLASH; TIME] ->
      let unary_ast , unary_l = unary t in
      aux ((h, unary_ast ) :: res) unary_l 
    | _ -> 
      begin
      match res with
      | [(_, x)] -> x, aux_l
      | _ -> Factor (List.rev res), aux_l
      end
  in
  aux [({lexeme = EMPTY; value = STRING ""; line = -1}, unary_ast)] unary_l 

and unary l =
  let () = if debug then
    (print_endline "Parsing unary"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  match l with
  | [] -> Empty, []
  | h :: t when match_lexeme h [BANG; MINUS] ->
    let unary_ast, unary_l = unary t in
    Unary (h, unary_ast), unary_l
  | _ -> call l

and call l =
  let () = if debug then
    (print_endline "Parsing call"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let rec arguments res aux_l =
    match aux_l with
    | [] -> List.rev res, aux_l
    | h :: t when match_lexeme h [D_INF] -> List.rev res, t
    | _ -> 
      let expr_ast, expr_l = expression aux_l in
      arguments (expr_ast :: res) expr_l
  in
  let primary_ast, primary_l = primary l in
  let rec aux res aux_l = 
    match aux_l with
    | h :: t when match_lexeme h [D_SUP] ->
      let arguments_ast, arguments_l = arguments [] t in
      aux (arguments_ast :: res) arguments_l 
    | h :: t when match_lexeme h [SUP_INF] ->
      let argument_ast, argument_l = expression t in
      aux ([argument_ast] :: res) argument_l
    | _ -> 
      begin
      match res with
      | [] -> primary_ast, primary_l
      | _ -> Call (primary_ast, List.rev res), aux_l
      end
  in
  aux [] primary_l

and primary l =
  let () = if debug then
    (print_endline "Parsing primary"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  match l with
  | [] -> Empty, l
  | h :: t when match_lexeme h [NUMBER] -> Number h, t
  | h :: t when match_lexeme h [STRING] -> String h, t
  | h :: t when match_lexeme h [ID] ->
    begin
    match Token.string_of_value h.value with
    | "true" -> True, t
    | "false" -> False, t
    | "null" -> Null, t
    | _ -> Id h, t
    end
  | h :: t when match_lexeme h [LEFT_PAREN] ->
    begin
    let expr_ast, expr_l = expression t in
    match expr_l with
    | [] -> raise (Parsing_error "Missing parenthesis at end of document")
    | h2 :: t2 when match_lexeme h2 [RIGHT_PAREN] ->
      ParenExpression expr_ast, t2
    | _ -> raise (Parsing_error "Unclosed parenthesised expression")
    end
  | h :: _ -> raise (Parsing_error ("Unexpected token: " ^ Token.string_of_lexeme h.lexeme))

let parse (l : token list) =
  programme [] l
