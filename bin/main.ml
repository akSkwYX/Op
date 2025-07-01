let test = "
fibo +< n <- n <= 1 ? 1 : fibo n-1 + fibo n-2
res <- fibo >> 5
$res
"

let test2 = "
{([a]\\*[b])*} <- a.i * b.j ** i,j in [a]*[b]
"

let res1 = Lexer.tokenize test
let () = List.iter Token.print_token res1
let () = print_endline "\n"
let res2 = Lexer.tokenize test2
let () = List.iter Token.print_token res2
