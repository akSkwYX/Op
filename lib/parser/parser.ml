open Ast
open Token_type

exception Parsing_error of string
exception Try_error

let debug = false

let match_lexeme ( token:token ) ( lexeme_list:lexeme list ) = List.mem token.lexeme lexeme_list

type precedence = {
  self : int;
  table : ( Token_type.lexeme * (precedence -> token list -> ast * precedence * token list) ) list array;
  op_prec_table : (Token_type.lexeme, int) Hashtbl.t;
}

let change_prec prec token new_prec =
  let remove_from_list l =
    let l, f =
      List.fold_left (fun (acc_l, acc_f) (lexeme, f) ->
        if lexeme = token.lexeme then
          (acc_l, (lexeme, f))
        else
          ((lexeme, f) :: acc_l, acc_f)
      ) ([], (EMPTY, fun _ _ -> Empty, prec, [])) l
    in List.rev l, f
  in
  let new_table = Array.copy prec.table in
  let new_op_prec_table = Hashtbl.copy prec.op_prec_table in
  let int_prec = Hashtbl.find prec.op_prec_table token.lexeme in
  let new_l, elem = remove_from_list new_table.(int_prec) in
  let () = new_table.(int_prec) <- new_l; new_table.(new_prec) <- (elem :: new_table.(new_prec));
           Hashtbl.replace new_op_prec_table token.lexeme new_prec in
  { self = prec.self; table = new_table; op_prec_table = new_op_prec_table }

let next_prec prec =
  let rec aux i =
    if i >= Array.length prec.table then
      raise (Parsing_error "No more precedence level available")
    else
      match prec.table.(i) with
      | [] -> aux (i + 1)
      | _ -> { self = i; table = prec.table; op_prec_table = prec.op_prec_table }
  in
  aux (prec.self + 1)

let previous_prec prec =
  let rec aux i =
    if i < 0 then
      raise (Parsing_error "No previous precedence level available")
    else
      match prec.table.(i) with
      | [] -> aux (i - 1)
      | _ -> { self = i; table = prec.table; op_prec_table = prec.op_prec_table }
  in
  aux (prec.self - 1)

let select_prec prec i =
  if i < 0 || i >= Array.length prec.table || prec.table.(i) = [] then
    raise (Parsing_error "Invalid precedence level")
  else
    { self = i; table = prec.table; op_prec_table = prec.op_prec_table }

let call_next_parse prec =
  let new_prec = next_prec prec in
  snd (List.hd new_prec.table.(new_prec.self)) new_prec

let call_previous_parse prec =
  let new_prec = previous_prec prec in
  snd (List.hd new_prec.table.(new_prec.self)) new_prec

let call_self_parse prec =
  if prec.self < 0 || prec.self >= Array.length prec.table || prec.table.(prec.self) = [] then
    raise (Parsing_error "Invalid precedence level")
  else
    snd (List.hd prec.table.(prec.self)) prec

let call_select_parse prec i =
  let new_prec = select_prec prec i in
  snd (List.hd new_prec.table.(new_prec.self)) new_prec

let rec programme res prec l =
  let prec = {self = 0; table = prec.table; op_prec_table = prec.op_prec_table} in
  match l with
  | [] -> 
    begin
    match res with
    | [] -> Empty
    | _ -> Prog (List.rev res)
    end
  | _ -> 
    let (decl_ast, decl_prec, decl_l) = call_next_parse prec l in
    programme (decl_ast :: res) decl_prec decl_l

and declaration prec l =
  let () = if debug then
    (print_endline "Parsing declaration"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  match l with
  | [] -> Empty, prec, l
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
      let (decl_ast, _, decl_l) = declaration prec t2 in
      (match decl_l with
      | h :: t when match_lexeme h [SEMICOLON] ->
        Declaration (id, params_list, decl_ast), prec, t
      | _ -> raise (Parsing_error ("Missing semicolon after declaration : " ^
                    Token.string_of_value id.value ^
                    (List.fold_left (fun acc token -> acc ^ " " ^ Token.string_of_value token.value) 
                                    "" params_list)
                    ^ (List.fold_left (fun acc token -> acc ^ " " ^ Token.string_of_value token.value) ""
                    (List.take (List.length params_l - List.length decl_l) params_l)))))
    | _ -> call_next_parse prec l
    end
  | _ -> call_next_parse prec l

and statement prec l =
  (*Need to make a huge optimization on this function currently very unefficient*)
  let () = if debug then
    (print_endline "Parsing statement"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  try printStmt prec l with
  | Try_error ->
    let () = if debug then
      print_endline "printStmt failed, trying whileStmt"
    in
    try whileStmt prec l with
    | Try_error ->
      let () = if debug then
        print_endline "whileStmt failed, trying block"
      in
      try block prec l with
      | Try_error ->
        let () = if debug then
          print_endline "block failed, trying ifStmt"
        in
        try ifStmt prec l with
        | Try_error ->
          let () = if debug then
            print_endline "ifStmt failed, trying expression"
          in
          try expression prec l with
            | Try_error ->
                raise (Parsing_error ("Unrecognized statement or expression" ^
                       List.fold_left (fun acc token -> acc ^ " " ^ Token.string_of_value token.value) "" l))

and ifStmt prec l =
  let () = if debug then
    (print_endline "Parsing if statement"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let expr_ast, _, expr_l = call_next_parse prec l in
  match expr_l with
  | [] -> raise Try_error
  | h :: t when match_lexeme h [QUESTION_MARK] ->
    begin
    let stmt_true_ast, _, stmt_true_l = call_self_parse prec t in
    match stmt_true_l with
    | [] -> raise ( Parsing_error "Unfinished if statement" )
    | h2 :: t2 when match_lexeme h2 [COLON] ->
      let stmt_false_ast, _, stmt_false_l = call_self_parse prec t2 in
      If (expr_ast, stmt_true_ast, stmt_false_ast), prec, stmt_false_l
    | _ -> raise ( Parsing_error "Missing false consequence in if statement" )
    end
  | _ -> raise Try_error

and whileStmt prec l =
  let () = if debug then
    (print_endline "Parsing while statement"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let stmt_ast, _, stmt_l = try block prec l with
  | Try_error ->
    let () = if debug then
      print_endline "block failed, trying expression"
    in
    call_next_parse prec l
  in
  match stmt_l with
  | [] -> raise Try_error
  | h :: t when match_lexeme h [TIME_TIME] ->
    let expr_ast, _, expr_l = call_next_parse prec t in
    While (expr_ast, stmt_ast), prec, expr_l 
  | _ -> raise Try_error

and printStmt prec l =
  let () = if debug then
    (print_endline "Parsing print statement"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  match l with
  | [] -> Empty, prec, l
  | h :: t when match_lexeme h [DOLLAR] ->
    let expr_ast, _, expr_l = call_next_parse prec t in
    Print expr_ast, prec, expr_l
  | _ -> raise Try_error

and block prec l =
  let () = if debug then
    (print_endline "Parsing block statement"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  match l with
  | [] -> Empty, prec, l
  | h :: t when match_lexeme h [LEFT_BRACE] ->
    let rec decl_star res aux_prec aux_l =
      match aux_l with
      | [] -> raise (Parsing_error "Unclosed block at end of file")
      | h :: t when match_lexeme h [RIGHT_BRACE] ->
        Block (List.rev res), prec, t
      | _ ->
        let decl_ast, decl_prec, decl_l = call_select_parse aux_prec 1 aux_l in
        decl_star (decl_ast :: res) decl_prec decl_l
    in
    decl_star [] prec t
  | _ -> raise Try_error

and expression prec l =
  let () = if debug then
    (print_endline "Parsing expression"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  match l with
  | [] -> Empty, prec, l
  | h :: h2 :: n :: t when match_lexeme h2 [BANG_INF] ->
    begin
    match n.lexeme with
    | NUMBER -> 
      let new_prec = int_of_float (Token.float_of_value n.value) in
      Empty, change_prec prec h new_prec, t
    | _ -> raise (Parsing_error "Expected a number after '!<'")
    end
  | _ -> call_next_parse prec l

and logic_or prec l =
  let () = if debug then
    (print_endline "Parsing logic or"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let logic_and_ast, _, logic_and_l = call_next_parse prec l in
  let rec aux res aux_l =
    match aux_l with
    | h :: t when match_lexeme h [D_VERT_BAR] ->
      let logic_and_ast, _, logic_and_l = call_next_parse prec t in
      aux (logic_and_ast :: res) logic_and_l
    | _ -> 
      begin
      match res with
      | [x] -> x, prec, aux_l
      | _ -> LogicOr (List.rev res), prec, aux_l
      end
  in
  aux [logic_and_ast] logic_and_l

and logic_and prec l =
  let () = if debug then
    (print_endline "Parsing logic_and"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let equality_ast, _, equality_l = call_next_parse prec l in
  let rec aux res aux_l =
    match aux_l with
    | h :: t when match_lexeme h [D_AMPERSAND] ->
      let equality_ast, _, equality_l = call_next_parse prec t in
      aux (equality_ast :: res) equality_l
    | _ -> 
      begin
      match res with
      | [x] -> x, prec, aux_l
      | _ -> LogicAnd (List.rev res), prec, aux_l
      end
  in
  aux [equality_ast] equality_l

and equality prec l =
  let () = if debug then
    (print_endline "Parsing equality"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let comparison_ast, _, comparison_l = call_next_parse prec l in
  let rec aux res aux_l =
    match aux_l with
    | h :: t when match_lexeme h [BANG_EQUAL; EQUAL] ->
      let comparison_ast, _, comparison_l = call_next_parse prec t in
      aux ((h, comparison_ast ) :: res) comparison_l
    | _ -> 
      begin
      match res with
      | [(_, x)] -> x, prec, aux_l
      | _ -> Equality (List.rev res), prec, aux_l
      end
  in
  aux [({lexeme = EMPTY; value = STRING ""; line = -1}, comparison_ast)] comparison_l

and comparison prec l =
  let () = if debug then
    (print_endline "Parsing comparison"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let term_ast, _, term_l = call_next_parse prec l in
  let rec aux res aux_l = 
    match aux_l with
    | h :: t when match_lexeme h [SUP; SUP_EQUAL; INF; INF_EQUAL] ->
      let term_ast, _, term_l = call_next_parse prec t in
      aux ((h, term_ast ) :: res) term_l 
    | _ -> 
      begin
      match res with
      | [(_, x)] -> x, prec, aux_l
      | _ -> Comparison (List.rev res), prec, aux_l
      end
  in
  aux [({lexeme = EMPTY; value = STRING ""; line = -1}, term_ast)] term_l 

and term prec l =
  let () = if debug then
    (print_endline "Parsing term"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let factor_ast, _, factor_l = call_next_parse prec l in
  let rec aux res aux_l = 
    match aux_l with
    | h :: t when match_lexeme h [MINUS; PLUS] ->
      let factor_ast, _, factor_l = call_next_parse prec t in
      aux ((h, factor_ast ) :: res) factor_l 
    | _ -> 
      begin
      match res with
      | [(_, x)] -> x, prec, aux_l
      | _ -> Term (List.rev res), prec, aux_l
      end
  in
  aux [({lexeme = EMPTY; value = STRING ""; line = -1}, factor_ast)] factor_l 

and factor prec l =
  let () = if debug then
    (print_endline "Parsing factor"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let unary_ast, _, unary_l = call_next_parse prec l in
  let rec aux res aux_l = 
    match aux_l with
    | h :: t when match_lexeme h [SLASH; TIME] ->
      let unary_ast, _, unary_l = call_next_parse prec t in
      aux ((h, unary_ast ) :: res) unary_l 
    | _ -> 
      begin
      match res with
      | [(_, x)] -> x, prec, aux_l
      | _ -> Factor (List.rev res), prec, aux_l
      end
  in
  aux [({lexeme = EMPTY; value = STRING ""; line = -1}, unary_ast)] unary_l 

and unary prec l =
  let () = if debug then
    (print_endline "Parsing unary"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  match l with
  | [] -> Empty, prec, []
  | h :: t when match_lexeme h [BANG; MINUS] ->
    let unary_ast, _, unary_l = call_self_parse prec t in
    Unary (h, unary_ast), prec, unary_l
  | _ -> call_next_parse prec l

and call prec l =
  let () = if debug then
    (print_endline "Parsing call"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  let rec arguments res aux_l =
    match aux_l with
    | [] -> List.rev res, aux_l
    | _ -> 
      try let primary_ast, _, primary_l = call_next_parse prec aux_l in
          arguments (primary_ast :: res) primary_l
      with 
      | Try_error -> List.rev res, aux_l
  in
  let primary_ast, _, primary_l = call_next_parse prec l in
  let rec aux res aux_l = 
    match aux_l with
    | h :: t when match_lexeme h [D_SUP] ->
      let arguments_ast, arguments_l = arguments [] t in
      aux (arguments_ast :: res) arguments_l 
    | _ -> 
      begin
      match res with
      | [] -> primary_ast, prec, primary_l
      | _ -> Call (primary_ast, List.rev res), prec, aux_l
      end
  in
  aux [] primary_l

and primary prec l =
  let () = if debug then
    (print_endline "Parsing primary"; print_newline ();
    List.iter Token.print_token l; print_newline ())
  in
  match l with
  | [] -> Empty, prec, l
  | h :: t when match_lexeme h [NUMBER] -> Number h, prec, t
  | h :: t when match_lexeme h [STRING] -> String h, prec, t
  | h :: t when match_lexeme h [ID] ->
    begin
    match Token.string_of_value h.value with
    | "true" -> True
    | "false" -> False
    | "null" -> Null
    | _ -> Id h
    end, prec, t
  | h :: t when match_lexeme h [LEFT_PAREN] ->
    begin
    let expr_ast, _, expr_l = call_select_parse prec 3 t in
    match expr_l with
    | [] -> raise (Parsing_error "Missing parenthesis at end of document")
    | h2 :: t2 when match_lexeme h2 [RIGHT_PAREN] ->
      ParenExpression expr_ast, prec, t2
    | _ -> raise (Parsing_error "Unclosed parenthesised expression")
    end
  | _ -> raise Try_error
  (* | h :: _ -> raise (Parsing_error ("Unexpected token: " ^ Token.string_of_lexeme h.lexeme)) *)

let parse (l : token list) =
  let prec = {
    self = 0;
    table = [|
      [];
      [( LEFT_ARROW, declaration )];
      [( S, statement )];
      [( E, expression )];
      [( D_VERT_BAR, logic_or )];
      [( D_AMPERSAND, logic_and )];
      [( EQUAL, equality )];
      [( SUP, comparison )];
      [( PLUS, term )];
      [( TIME, factor )];
      [( BANG, unary )];
      [( D_SUP, call )];
      [( P, primary )]
    |];
    op_prec_table = Hashtbl.create 12
  }
  in
  let () =
    Hashtbl.add prec.op_prec_table LEFT_ARROW 1;
    Hashtbl.add prec.op_prec_table S 2;
    Hashtbl.add prec.op_prec_table E 3;
    Hashtbl.add prec.op_prec_table D_VERT_BAR 4;
    Hashtbl.add prec.op_prec_table D_AMPERSAND 5;
    Hashtbl.add prec.op_prec_table EQUAL 6;
    Hashtbl.add prec.op_prec_table SUP 7;
    Hashtbl.add prec.op_prec_table PLUS 8;
    Hashtbl.add prec.op_prec_table TIME 9;
    Hashtbl.add prec.op_prec_table BANG 10;
    Hashtbl.add prec.op_prec_table D_SUP 11;
    Hashtbl.add prec.op_prec_table P 12
  in
  programme [] prec l
