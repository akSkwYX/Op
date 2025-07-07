(* TODO :
   - Change list type to enable adding at end in O(1) 
   - Add panic mode to parser to handle multi error
*)

open Ast
open Token_type

exception Parsing_error of string

let match_lexeme token lexeme_list = List.mem token.lexeme lexeme_list

let rec programme ast l =
  match l with
  | [] ->
    match ast with
    | Empty -> Empty
    | Prog children -> Prog (List.rev children)
  | _ ->
    let (ast', l') = declaration l in
    match ast with
    | Empty -> aux (Prog [ast']) l'
    | Prog children -> aux (Prog [ast'::children]) l'

and declaration ast l =
  match l with
  | [] -> ast
  | h :: t when match_lexeme h [ID] ->
    match t with
    | [] -> raise (Parsing_error "[" ^ h.line ^ "] Unfinished declaration")


let parse (l : token list) =
  programme Empty l
