open Token_type

let string_of_lexeme = function
  | LEFT_ARROW -> "LEFT_ARROW"
  | PLUS_INF -> "PLUS_INF"
  | MINUS_INF -> "MINUS_INF"
  | SUP_SUP -> "SUP_SUP"
  | DOLL -> "DOLL"
  | BANG -> "BANG"
  | BANG_INF -> "BANG_INF"
  | QUEST_MARK -> "QUEST_MARK"
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
  | NUMBER -> "NUMBER"
  | STRING -> "STRING"
  | ID -> "ID"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | LEFT_SQ_BRACK -> "LEFT_SQ_BRACK"
  | RIGHT_SQ_BRACK -> "RIGHT_SQ_BRACK"
  | UNDERSCORE -> "UNDERSCORE"
  | SLASH -> "SLASH"
  | BACKSLASH -> "BACKSLASH"
  | AMPERSAND -> "AMPERSAND"
  | AMPERSAND_AMPERSAND -> "AMPERSAND_AMPERSAND"
  | VERT_BAR -> "VERT_BAR"
  | VERT_BAR_VERT_BAR -> "VERT_BAR_VERT_BAR"
  | CARET -> "CARET"
  | DOT -> "DOT"
  | COMMA -> "COMMA"
  | LINE_BREAK -> "LINE_BREAK"

let string_of_value = function
  | STRING s -> s 
  | FLOAT f -> string_of_float f

let print_token t =
  print_endline (string_of_lexeme t.lexeme ^ " : " ^ string_of_value t.value)
