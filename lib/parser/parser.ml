open Ast
open Token_type

exception Parsing_error of string

let match_lexeme ( token:token ) ( lexeme_list:lexeme list ) = List.mem token.lexeme lexeme_list

let rec programme ast l =
  match l with
  | [] ->
    begin
    match ast with
    | Empty -> Empty
    | Prog children -> Prog (List.rev children)
    end
  | _ ->
    begin
    let (ast', l') = declaration l in
    match ast with
    | Empty -> aux (Prog [ast']) l'
    | Prog children -> aux (Prog [ast'::children]) l'
    end

and declaration ast l =
  match l with
  | [] -> ast
  | id :: larr :: next :: t when match_lexeme id [ID] && match_lexeme larr [LEFT_ARROW] ->
    begin
    if match_lexeme next [LEFT_PAREN] then
      begin
      let rec aux res l' =
        match l' with
        | [] -> raise (Parsing_error "Missing parenthesis at end of program")
        | h :: t2 when match_lexeme h [RIGHT_PAREN] -> Declaration id res , t2
        | _ -> let (ast', l'') = declaration ast l' in aux (res @ [ast']) l''
      in aux [] t
      end
    else
      let (ast', l') = declaration ast (next :: t) in
      Declaration id [ast'] , l'
    end
  | _ -> statement ast l


let parse (l : token list) =
  programme Empty l
