open Token_type

let string_of_lexeme = function
  | LEFT_ARROW -> "LEFT_ARROW"
  | D_SUP -> "D_SUP"
  | DOLLAR -> "DOLLAR"
  | BANG -> "BANG"
  | QUESTION_MARK -> "QUESTION_MARK"
  | COLON -> "COLON"
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | TIME_TIME -> "TIME_TIME"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIME -> "TIME"
  | INF -> "INF"
  | INF_EQUAL -> "INF_EQUAL"
  | SUP -> "SUP"
  | SUP_EQUAL -> "SUP_EQUAL"
  | EQUAL -> "EQUAL"
  | BANG_EQUAL -> "BANG_EQUAL"
  | NUMBER -> "NUMBER"
  | STRING -> "STRING"
  | ID -> "ID"
  | SLASH -> "SLASH"
  | D_AMPERSAND -> "D_AMPERSAND"
  | D_VERT_BAR -> "D_VERT_BAR"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | SUP_INF -> "SUP_INF"
  | EMPTY -> "EMPTY"
  | D_INF -> "D_INF"

let string_of_value = function
  | STRING s -> s 
  | FLOAT f -> string_of_float f

let float_of_value = function
  | FLOAT f -> f
  | _ -> failwith "float_of_value : Expected a FLOAT value"

let print_token t =
  print_endline (string_of_lexeme t.lexeme ^ " : " ^ string_of_value t.value)
