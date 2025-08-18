open Ast
open Token_type

let debug = false

exception InterpreterError of string

type environment = {
  enclosing : environment option;
  variables : (string, value) Hashtbl.t;
  functions : (string, token list * ast) Hashtbl.t;
}

and value =
  | Bool of bool
  | String of string
  | Number of float
  | Null
  | Empty of unit
  | Id of string
  | Block of environment

let create_environment enclosing =
  { enclosing = enclosing;
    variables = Hashtbl.create 10;
    functions = Hashtbl.create 10 }

let rec find_variable env id =
  try Hashtbl.find env.variables id with
  | Not_found ->
    match env.enclosing with
    | Some enclosing_env -> find_variable enclosing_env id
    | None -> failwith ("Variable not found : " ^ id)

let rec find_function env id =
  try Hashtbl.find env.functions id with
  | Not_found ->
    match env.enclosing with
    | Some enclosing_env -> find_function enclosing_env id
    | None -> failwith ("Function not found : " ^ id)

let add_var env id value =
  Hashtbl.replace env.variables id value

let add_function env id args body =
  Hashtbl.replace env.functions id (args, body)

let rec string_of_value = function
  | Bool b -> string_of_bool b
  | String s -> s
  | Number n -> string_of_float n
  | Null -> "null"
  | Empty () -> "empty"
  | Id id -> id
  | Block env ->
    "Block :\n" ^
    (Hashtbl.fold (fun k v acc -> acc ^ k ^ " : " ^ string_of_value v ^ "\n") env.variables "") ^
    (Hashtbl.fold (fun k _ acc -> acc ^ k ^ " : fun\n") env.functions "")

(* let add_var env var (body : Ast.ast) = *)
(*   let rec replace_in_var ( body : Ast.ast ) : Ast.ast = *)
(*     match body with *)
(*     | Id id -> *)
(*       (try Hashtbl.find env.variables (Token.string_of_value id.value) with *)
(*       | Not_found -> body) *)
(*     | Empty | True | False | Null | String _ | Number _ -> body *)
(*     | ParenExpression child -> ParenExpression (replace_in_var child) *)
(*     | Unary (op, child) -> Unary (op, replace_in_var child) *)
(*     | Call (called, args) -> Call (replace_in_var called, List.map (List.map replace_in_var) args) *)
(*     | Factor children -> Factor (List.map (fun (t, child) -> (t, replace_in_var child)) children) *)
(*     | Term children -> Term (List.map (fun (t, child) -> (t, replace_in_var child)) children) *)
(*     | Comparison children -> Comparison (List.map (fun (t, child) -> (t, replace_in_var child)) children) *)
(*     | Equality children -> Equality (List.map (fun (t, child) -> (t, replace_in_var child)) children) *)
(*     | LogicAnd children -> LogicAnd (List.map replace_in_var children) *)
(*     | LogicOr children -> LogicOr (List.map replace_in_var children) *)
(*     | If (cond, then_branch, else_branch) -> If (replace_in_var cond, replace_in_var then_branch, replace_in_var else_branch) *)
(*     | While (cond, body) -> While (replace_in_var cond, replace_in_var body) *)
(*     | Print child -> Print (replace_in_var child) *)
(*     | Block statements -> Block (List.map replace_in_var statements) *)
(*     | Declaration (t, params, body) -> Declaration (t, params, replace_in_var body) *)
(*     | Prog declarations -> Prog (List.map replace_in_var declarations) *)
(*   in *)
(*   try Hashtbl.replace env.variables (Token.string_of_value var.value) (replace_in_var body) with *)
(*   | Not_found -> failwith ("Variable not found : " ^ Token.string_of_value var.value) *)

(* let add_var env var value = *)
(*   Hashtbl.replace env.variables (Token.string_of_value var.value) value *)

let rec interpret ( env : environment ) ( ast : Ast.ast ) =
  let () = if debug then 
    (print_endline ("Interpreting :\n" ^ Ast.string_of_ast ast);
    print_endline ("Current environment :\n" ^
    (Hashtbl.fold (fun k v acc -> acc ^ k ^ " : " ^ string_of_value v ^ "\n") env.variables ""))) in
    (*^ (Hashtbl.fold (fun k (args, body) acc -> acc ^ k ^ " " ^ List.fold_left (fun acc arg -> acc ^ Token.string_of_value arg.value ) "" args ^ " : " ^ Ast.string_of_ast body ^ "\n") env.functions ""))) in *)
  let env, value =
    match ast with
    | Empty -> env, Empty ()
    | True -> env, Bool true
    | False -> env, Bool false
    | Null -> env, Null
    | String t -> env, String ( Token.string_of_value t.value )
    | Number t -> env, Number ( Token.float_of_value t.value )
    (* | Id t -> (try interpret env (Hashtbl.find env.variables (Token.string_of_value t.value)) with *)
    (*   | Not_found -> env, Id (Token.string_of_value t.value)) *)
    | Id t -> (try env, find_variable env (Token.string_of_value t.value) with
      | Failure _ -> env, Id (Token.string_of_value t.value))
    | ParenExpression child -> interpret env child
    | Unary (t, child) -> interpret_unary env t child
    | Call (called, arguments_list_list) -> interpret_call env called arguments_list_list
    | Factor children -> interpret_factor env children
    | Term children -> interpret_term env children
    | Comparison children -> interpret_comparison env children
    | Equality children -> interpret_equality env children
    | LogicAnd children -> interpret_logic_and env children
    | LogicOr children -> interpret_logic_or env children
    | If (condition, then_branch, else_branch) -> interpret_if env condition then_branch else_branch
    | While (condition, body) -> interpret_while env condition body
    | Print child -> interpret_print env child
    | Block statements -> interpret_block env statements
    | Declaration (t, params, body) -> interpret_declaration env t params body
    | Prog declarations -> interpret_prog env declarations
  in
  let () = if debug then
    (print_endline ("Result : " ^ string_of_value value)) in
  env, value

and interpret_unary env t child =
  let env, child_value = interpret env child in
  match t.lexeme with
  | Token_type.BANG ->
      (match child_value with
       | Bool b -> env, Bool (not b)
       | _ -> failwith "Unary operator '!' expects a boolean value")
  | Token_type.MINUS ->
      (match child_value with
       | Number n -> env, Number (-. n)
       | _ -> failwith "Unary operator '-' expects a number value")
  | _ -> failwith ("interpret_unary : Unreachable")

and interpret_call env called arguments_list_list =
  (* let rec interpret_arguments env args = *)
  (*   match args with *)
  (*   | [] -> env, [] *)
  (*   | arg :: rest -> *)
  (*       let env, value = interpret env arg in *)
  (*       let env_rest, values_rest = interpret_arguments env rest in *)
  (*       env_rest, value :: values_rest *)
  (* in *)
  let env, called_value = interpret env called in
  let rec aux env called_value ( arguments_list_list : ast list list ) =
    match arguments_list_list with
    | [] ->
      (match called_value with
      | Id name ->
        (* interpret env (try Hashtbl.find env.variables name with *)
        (* | Not_found -> failwith ("Variable '" ^ name ^ "' not found")) *)
        env, find_variable env name
      | x -> env, x)
    | arguments_list :: rest ->
      match called_value with
      | Id name -> 
        let args, body = find_function env name in
        let func_env = create_environment (Some env) in
        let () = List.iter2 (fun arg param ->
          add_var func_env (Token.string_of_value arg.value) (interpret env param |> snd)
        ) args arguments_list in
        let _, value = interpret func_env body in
        aux env value rest
      | Block block_env ->
        begin
        match arguments_list with
        | [] -> failwith "Missing arguments for block call"
        | [Id arg] -> 
            begin
            let id = Token.string_of_value arg.value in
            try env, find_variable block_env id with
              | Failure _ -> 
                begin
                let args, body = try find_function block_env id with
                  | Failure _ -> failwith ("Variable or function '" ^ id ^ "' not found") in
                let func_env = create_environment (Some block_env) in
                let () = List.iter2 (fun arg param ->
                  add_var func_env (Token.string_of_value arg.value) (interpret env param |> snd)
                ) args arguments_list in
                let _, value = interpret func_env body in
                aux env value rest
                end
            end
        | _ -> failwith "Block call must have a single identifier argument"
        end
      | _ -> failwith "Called value must be an identifier representing a function"
  in
  aux env called_value arguments_list_list

and interpret_factor env children =
  match children with
  | [] -> failwith "interpret_factor : Expected at least one child"
  | (_, child) :: rest ->
    let env, first_value = interpret env child in
    match first_value with
    | Number n ->
      let env, n = 
      List.fold_left (fun (env, acc) (token, child)->
        let env, value = interpret env child in
        env, match value with
        | Number n ->
          (match token.lexeme with
          | Token_type.SLASH -> acc /. n
          | Token_type.TIME -> acc *. n
          | _ -> failwith "interpret_factor : Unreachable")
        | _ -> failwith "interpret_factor : Expected a number value"
      ) (env, n) rest
      in
      env, Number n
    | _ -> failwith "interpret_factor : Expected a number value"

and interpret_term env children =
  match children with
  | [] -> failwith "interpret_term : Expected at least one child"
  | (_, child) :: rest ->
    let env, first_value = interpret env child in
    match first_value with
    | Number n ->
      let env, n = 
      List.fold_left (fun (env, acc) (token, child)->
        let env, value = interpret env child in
        env, match value with
        | Number n ->
          (match token.lexeme with
          | Token_type.PLUS -> acc +. n
          | Token_type.MINUS -> acc -. n
          | _ -> failwith "interpret_term : Unreachable")
        | _ -> failwith "interpret_term : Expected a number value"
      ) (env, n) rest
      in
      env, Number n
    | _ -> failwith "interpret_term : Expected a number value"

and interpret_comparison env children =
  match children with
  | [] -> failwith "interpret_comparison : Expected at least one child"
  | (_, child) :: rest ->
    let env, first_value = interpret env child in
    let env, _, b = 
    List.fold_left (fun (env, last_value, res) (token, child)->
      let env, value = interpret env child in
      env, value, match last_value, value with
      | Number n1, Number n2 ->
        (match token.lexeme with
        | Token_type.SUP -> res && (n1 > n2)
        | Token_type.SUP_EQUAL -> res && (n1 >= n2)
        | Token_type.INF -> res && (n1 < n2)
        | Token_type.INF_EQUAL -> res && (n1 <= n2)
        | _ -> failwith "interpret_comparison : Unreachable")
      | String s1, String s2 ->
        (match token.lexeme with
        | Token_type.SUP -> res && String.compare s1 s2 > 0
        | Token_type.SUP_EQUAL -> res && String.compare s1 s2 >= 0
        | Token_type.INF -> res && String.compare s1 s2 < 0
        | Token_type.INF_EQUAL -> res && String.compare s1 s2 <= 0
        | _ -> failwith "interpret_comparison : Unreachable")
      | Number _, String _ | String _, Number _ ->
        failwith "interpret_comparison : Cannot compare number and string"
      | _ -> failwith "interpret_comparison : Expected a number or string value"
    ) (env, first_value, true) rest
    in
    env, Bool b

and interpret_equality env children =
  match children with
  | [] -> failwith "interpret_equality : Expected at least one child"
  | (_, child) :: rest ->
    let env, first_value = interpret env child in
    let env, _, b = 
    List.fold_left (fun (env, last_value, res) (token, child)->
      let env, value = interpret env child in
      env, value, match last_value, value with
      | Number n1, Number n2 ->
        (match token.lexeme with
        | Token_type.EQUAL -> res && (n1 = n2)
        | Token_type.BANG_EQUAL -> res && (n1 = n2)
        | _ -> failwith "interpret_equality : Unreachable")
      | String s1, String s2 ->
        (match token.lexeme with
        | Token_type.EQUAL -> res && String.compare s1 s2 = 0
        | Token_type.BANG_EQUAL -> res && String.compare s1 s2 <> 0
        | _ -> failwith "interpret_equality : Unreachable")
      | Bool b1, Bool b2 ->
        (match token.lexeme with
        | Token_type.EQUAL -> res && (b1 = b2)
        | Token_type.BANG_EQUAL -> res && (b1 <> b2)
        | _ -> failwith "interpret_equality : Unreachable")
      | Null, Null ->
        (match token.lexeme with
        | Token_type.EQUAL -> res && true
        | Token_type.BANG_EQUAL -> res && false
        | _ -> failwith "interpret_equality : Unreachable")
      | _ -> 
        (match token.lexeme with
        | Token_type.EQUAL -> res && false
        | Token_type.BANG_EQUAL -> res && true
        | _ -> failwith "interpret_equality : Unreachable")
    ) (env, first_value, true) rest
    in
    env, Bool b

and interpret_logic_and env children =
  let env, res =
    List.fold_left (fun (env, res) child ->
      let env, value = interpret env child in
      match value with
      | Bool b -> env, (res && b)
      | _ -> failwith "interpret_logic_and : Expected a boolean value"
    ) (env, true) children
  in
  env, Bool res

and interpret_logic_or env children =
  let env, res =
    List.fold_left (fun (env, res) child ->
      let env, value = interpret env child in
      match value with
      | Bool b -> env, (res || b)
      | _ -> failwith "interpret_logic_or : Expected a boolean value"
    ) (env, false) children
  in
  env, Bool res

and interpret_if env condition then_branch else_branch =
  let env, condition_value = interpret env condition in
  match condition_value with
  | Bool true -> interpret env then_branch
  | Bool false -> interpret env else_branch
  | _ -> failwith "interpret_if : Condition must evaluate to a boolean value"

and interpret_while env condition body =
  let rec loop loop_env =
    let loop_env, condition_value = interpret loop_env condition in
    match condition_value with
    | Bool true ->
      let loop_env, value = interpret loop_env body in
      (match value with
      | Block block_env ->
        loop block_env
      | _ -> loop loop_env)
    | Bool false -> env, Empty ()
    | _ -> failwith "interpret_while : Condition must evaluate to a boolean value"
  in
  loop env

and interpret_print env child =
  let env, value = interpret env child in
  match value with
  | String s -> print_string s; env, Empty ()
  | Number n -> (if float_of_int (int_of_float n) = n then
                  print_int (int_of_float n)
                else
                  print_float n)
                ; env, Empty ()
  | Bool b -> print_string (string_of_bool b); env, Empty ()
  | Null -> print_string "null"; env, Empty ()
  | _ -> failwith "interpret_print : Unsupported value type for printing"

and interpret_block env statements =
  let new_env = create_environment (Some env) in
  let block_env = List.fold_left (fun env statement ->
    let env, _ = interpret env statement in
    env
  ) new_env statements in
  env, Block block_env

and interpret_declaration env t params body =
  match t.lexeme with
  | Token_type.ID ->
    begin
    match params with
    | [] ->
      begin
      add_var env (Token.string_of_value t.value) ( interpret env body |> snd );
      env, Empty ()
      end
    | _ ->
      begin
      (* Will change parameters parsing to enable putting what we want as parameters (as long as it interpret
         to an Id) *)
      (* let env, paramsId = List.fold_left (fun (env, paramsId) param -> *)
      (*   let env, param_value = interpret env param in *)
      (*   match param_value with *)
      (*   | Id s -> env, (s::paramsId)  *)
      (*   | _ -> failwith "interpret_declaration : Expected an identifier for parameter" *)
      (* ) (env, []) params in *)
      let paramsId = List.map (fun param ->
        match param.lexeme with
        | Token_type.ID -> param
        | _ -> failwith "interpret_declaration : Expected an identifier for parameter"
      ) params in
      add_function env (Token.string_of_value t.value) paramsId body;
      env, Empty ()
      end
    end
  | _ -> failwith "interpret_declaration : Unreachable"

and interpret_prog env declarations =
  List.fold_left (fun env declaration ->
    let env, _ = interpret env declaration in
    env
  ) env declarations, Empty ()

let eval ast =
  let initial_env = create_environment None in
  let (env, _) = interpret initial_env ast in
  if debug then
    (print_endline ("Final environment :\n" ^
    (Hashtbl.fold (fun k v acc -> acc ^ k ^ " : " ^ string_of_value v ^ "\n") env.variables "") ^ "\n" ^
    (Hashtbl.fold (fun k (args, body) acc -> acc ^ k ^ " " ^ List.fold_left (fun acc arg -> acc ^ Token.string_of_value arg.value ) "" args ^ " : " ^ Ast.string_of_ast body ^ "\n") env.functions "")))
