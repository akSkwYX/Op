let test = "
fibo n <- n <= 1 ? 1 : fibo >> (n-1) << + fibo >> (n-2) <<
res <- fibo >< 5
$res
"

(* let test = " *)
(*   test i <- $i *)
(*   l <- 0 *)
(*   {test >< l *)
(*    l <- l + 1} ** l <= 5 *)
(* " *)

let token_list = Lexer.tokenize test
(* let () = print_endline "Lexing result :" *)
(* let () = List.iter Token.print_token token_list *)
(* let () = print_endline "\n" *)
let ast = Parser.parse token_list
(* let () = print_endline "Parsing result :" *)
(* let () = print_endline (Ast.string_of_ast ast) *)
(* let () = print_endline "\n" *)
let () = Interpreter.eval ast
