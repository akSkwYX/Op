open Token_type
open Ast

let rec declaration (t : token list) =
  let statement, statement_residue = statement t in
  match (List.hd left_member_residue).lexeme with
  | LEFT_ARROW ->
    let right_member, right_member_residue = declaration (List.tl left_member_residue) in
    let parsed_program, parsed_residue = declaration right_member_residue in
    match parsed_residue with
    | [] -> Declaration (left_member, )

