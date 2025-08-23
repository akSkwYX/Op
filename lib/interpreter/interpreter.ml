(* TODO : 
   - Change declaration to have tree as id (so we can modify array elements)
   - Unify functions and variables
   - Add comments
   - Add library support
   - Change error handling to use exceptions
*)

open Ast
open Token_type

let debug = false

exception Interpreting_error of string

type environment = {
  mutable enclosing : environment option;
  mutable variables : (string, value) Hashtbl.t;
  mutable functions : (string, string list * ast) Hashtbl.t;
}

and value =
  | Bool of bool
  | String of string
  | Number of float
  | Null
  | Empty of unit
  | Id of string
  | Block of environment
  | Array of value array

let create_environment enclosing =
  { enclosing = enclosing;
    variables = Hashtbl.create 10;
    functions = Hashtbl.create 10 }

let environment_copy env =
  let new_env = create_environment env.enclosing in
  new_env.variables <- Hashtbl.copy env.variables;
  new_env.functions <- Hashtbl.copy env.functions;
  new_env

let environment_union env1 env2 =
  let new_env = create_environment ( Some env2 ) in
  (match new_env.enclosing with
  | Some env -> env.enclosing <- ( Some env1 )
  | None -> failwith "environment_union : Unreachable");
  new_env

let environment_equal env1 env2 =
  env1.enclosing = env2.enclosing &&
  Hashtbl.length env1.variables = Hashtbl.length env2.variables &&
  Hashtbl.length env1.functions = Hashtbl.length env2.functions &&
  Hashtbl.fold (fun k v acc -> acc &&
    try v = Hashtbl.find env2.variables k with 
      | Not_found -> false 
  ) env1.variables true &&
  Hashtbl.fold (fun k v acc -> acc &&
    try v = Hashtbl.find env2.functions k with 
      | Not_found -> false 
  ) env1.functions true


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
  | Array arr ->
    "Array :\n[ " ^
    (Array.fold_left (fun acc v -> acc ^ string_of_value v ^ "; ") "" arr) ^
    "]"

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
    | Id t -> (try env, find_variable env (Token.string_of_value t.value) with
      | Failure _ -> env, Id (Token.string_of_value t.value))
    | Array children -> env, Array (Array.of_list (List.map (fun child -> interpret env child |> snd) children))
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
    | Declaration (head, params, body) -> interpret_declaration env head params body
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
       | Array a -> env, Array (Array.map (fun v -> match v with
          | Bool b -> Bool (not b)
          | _ -> raise (Interpreting_error "Unary operator '!' expects a boolean value array")) a)
       | _ -> failwith "Unary operator '!' expects a boolean value")
  | Token_type.MINUS ->
      (match child_value with
       | Number n -> env, Number (-. n)
       | Array a -> env, Array (Array.map (fun v -> match v with
          | Number n -> Number (-. n)
          | _ -> raise (Interpreting_error "Unary operator '-' expects a number value array")) a)
       | _ -> failwith "Unary operator '-' expects a number value")
  | _ -> failwith ("interpret_unary : Unreachable")

and aux_interpret_call env called_value arguments_list_list =
  match arguments_list_list with
  | [] ->
    (match called_value with
    | Id name ->
      env, find_variable env name
    | x -> env, x)
  | arguments_list :: rest ->
    match called_value with
    | Id name -> 
      let params, body = find_function env name in
      let func_env = create_environment (Some env) in
      let () = List.iter2 (fun param arg ->
        add_var func_env param (interpret env arg |> snd)
      ) params arguments_list in
      let _, value = interpret func_env body in
      aux_interpret_call env value rest
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
              let params, body = find_function block_env id in
              let func_env = create_environment (Some block_env) in
              let () = List.iter2 (fun param arg ->
                add_var func_env param (interpret env arg |> snd)
              ) params arguments_list in
              let _, value = interpret func_env body in
              aux_interpret_call env value rest
              end
          end
      | _ -> failwith "Block call must have a single identifier argument"
      end
    | Array a ->
      begin
      match arguments_list with
      | [] -> raise (Interpreting_error "Array call expects at least one argument")
      | [Number n] -> let index = int_of_float (Token.float_of_value n.value) in
          if index < 0 then
            raise (Interpreting_error "Array index cannot be negative")
          else if index >= Array.length a then
            raise (Interpreting_error "Array index out of bounds")
          else
            aux_interpret_call env (Array.get a index) rest
      | [Id id] -> let id_str = Token.string_of_value id.value in
          if id_str = "len" || id_str = "length" then
            aux_interpret_call env (Number (float_of_int (Array.length a))) rest
          else
            raise (Interpreting_error ("Unknown array method : " ^ id_str))
      | _ -> raise (Interpreting_error "Array call expects a single number or identifier argument")
      end
    | _ -> failwith "Called value must be an identifier representing a function"

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
  aux_interpret_call env called_value arguments_list_list

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
    | Array a ->
      List.fold_left (fun (env, acc) (token, child) ->
        let env, value = interpret env child in
        env, match token.lexeme with
        | Token_type.SLASH -> raise (Interpreting_error "Array does not support '/' operator")
        | Token_type.TIME ->
          (match value with
          | Number n -> (match acc with
            | Array [| v |] -> Array ( Array.make (int_of_float n) v )
            | Array arr_acc -> Array ( Array.make (int_of_float n) (Array arr_acc))
            | _ -> failwith "interpret_factor : Unreachable")
          | _ -> raise (Interpreting_error "Array can only be multiplied by a number"))
        | _ -> failwith "interpret_factor : Unreachable") (env, Array a) rest
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
    | String s ->
      let env, s =
      List.fold_left (fun (env, acc) (token, child) ->
        let env, value = interpret env child in
        env, match value with
        | String s ->
          (match token.lexeme with
          | Token_type.PLUS -> acc ^ s
          | Token_type.MINUS -> failwith "interpret_term : Cannot use '-' operator on strings"
          | _ -> failwith "interpret_term : Unreachable")
        | _ -> raise (Interpreting_error "Cannot add a string to a non-string value")
      ) (env, s) rest
      in
      env, String s
    | Block b_env ->
      let env, new_env =
        List.fold_left (fun (env, acc) (token, child) ->
          let env, value = interpret env child in
          env, match value with
          | Block b_env -> (match token.lexeme with
            | Token_type.PLUS -> environment_union acc b_env
            | Token_type.MINUS -> raise (Interpreting_error "Cannot use '-' operator on blocks")
            | _ -> failwith "interpret_term : Unreachable")
          | _ -> raise (Interpreting_error "Cannot add a block and a non-block value")
        ) (env, b_env) rest
      in
      env, Block new_env
    | Array a ->
      let env, new_array =
      List.fold_left (fun (env, acc) (token, child) ->
        let env, value = interpret env child in
        env, match value with
        | Array a -> (match token.lexeme with
          | Token_type.PLUS -> Array.append acc a
          | Token_type.MINUS -> Array.of_list (List.filter (fun v -> not (Array.mem v a)) ( Array.to_list acc ))
          | _ -> failwith "interpret_term : Unreachable")
        | _ -> raise (Interpreting_error "Cannot add or subtract an array and a non-array value")
      ) (env, a) rest
      in
      env, Array new_array
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
      | Block b1, Block b2 ->
        (match token.lexeme with
        | Token_type.EQUAL -> res && (environment_equal b1 b2)
        | Token_type.BANG_EQUAL -> res && not (environment_equal b1 b2)
        | _ -> failwith "interpret_equality : Unreachable")
      | Array a1, Array a2 ->
        (match token.lexeme with
        | Token_type.EQUAL -> res && (try a1 = a2 with | _ -> false)
        | Token_type.BANG_EQUAL -> res && (try a1 <> a2 with | _ -> true)
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
  | Array a -> print_string ("[ " ^
               (Array.fold_left (fun acc v -> acc ^ string_of_value v ^ "; ") "" a) ^ "]");
               env, Empty ()
  | _ -> failwith "interpret_print : Unsupported value type for printing"

and interpret_block env statements =
  let new_env = create_environment (Some env) in
  let block_env = List.fold_left (fun env statement ->
    let env, _ = interpret env statement in
    env
  ) new_env statements in
  env, Block block_env

and interpret_declaration env head params body =
  let aux_interpret_declaration env value params body =
    match value with
    | Id id ->
      begin
      match params with
      | [] ->
        add_var env id ( interpret env body |> snd );
        env, Empty ()
      | _ ->
        begin
        let paramsId = List.map (fun param ->
          let _, param_value = interpret env param in
          match param_value with
          | Id s -> s
          | _ -> raise (Interpreting_error "Parameter must evaluate to an identifier")
        ) params in
        add_function env id paramsId body;
        env, Empty ()
        end
      end
    | _ -> raise (Interpreting_error "Function or variable name must be an identifier")
  in
  match head with
  | Call (Id a, args) | ParenExpression (Call (Id a, args)) when (
      try match find_variable env (Token.string_of_value a.value) with | Array _ -> true | _ -> false with 
        | Failure _ -> false
    ) ->
      let rec aux current aux_args =
        match aux_args with
        | [] -> raise (Interpreting_error "Missing arguments for array element assignment")
        | [ast_arg] :: [] -> 
            let arg = interpret env ast_arg |> snd in
            (match arg with
            | Number n -> current.(int_of_float n) <- interpret env body |> snd; env, Empty ()
            | Id id -> (match find_variable env id with
            | Number n -> current.(int_of_float n) <- interpret env body |> snd; env, Empty ()
                | _ -> raise (Interpreting_error "Array index must be a number"))
            | _ -> raise (Interpreting_error "Array index must be a number"))
        | [ast_arg] :: rest -> 
            let aux n =
              if n < 0 then
                raise (Interpreting_error "Array index cannot be negative")
              else
                (match current.(n) with
                | Array a -> aux a rest
                | Block _ | Id _ -> 
                  let _, value = aux_interpret_call env current.(n) rest in
                  aux_interpret_declaration env value params body          
                | _ -> raise (Interpreting_error "Trying to call an uncallable value"))
            in
            let arg = interpret env ast_arg |> snd in
            (match arg with
            | Number n -> let index = int_of_float n in aux index
            | Id id -> (match find_variable env id with
                | Number n -> let index = int_of_float n in aux index
                | _ -> raise (Interpreting_error "Array index must be a number"))
            | _ -> raise (Interpreting_error "Array index must be a number"))
        | _ -> raise (Interpreting_error "Array index must be a single number")
      in
      let array = match find_variable env (Token.string_of_value a.value) with
        | Array a -> a
        | _ -> failwith "Unreachable"
      in
      aux array args
  | _ -> aux_interpret_declaration env (interpret env head |> snd) params body

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
    (Hashtbl.fold (fun k (args, body) acc -> acc ^ k ^ " " ^ List.fold_left (fun acc arg -> acc ^ arg ^ " ") "" args ^ ": " ^ Ast.string_of_ast body ^ "\n") env.functions "")))
