let file = try Sys.argv.(1) with _ -> failwith "Usage : ./main <filename>\ndune exec Op -- <filename>"

let script =
  let f = In_channel.open_bin file in
  let s = In_channel.input_all f in
  In_channel.close f; s

let token_list = Lexer.tokenize script
(* let () = print_endline "Lexing result :" *)
(* let () = List.iter Token.print_token token_list *)
(* let () = print_endline "\n" *)
let ast = Parser.parse token_list
(* let () = print_endline "Parsing result :" *)
(* let () = print_endline (Ast.string_of_ast ast) *)
(* let () = print_endline "\n" *)
let () = Interpreter.eval ast
let () = print_newline ()
