type lexeme =
  | LEFT_ARROW | SEMICOLON
  | S | QUESTION_MARK | COLON | TIME_TIME | DOLLAR | LEFT_BRACE | RIGHT_BRACE
  | E | BANG_INF
  | D_VERT_BAR | D_AMPERSAND | BANG_EQUAL | EQUAL
  | SUP | SUP_EQUAL | INF | INF_EQUAL
  | PLUS | MINUS | SLASH | TIME
  | BANG
  | D_SUP
  | P | LEFT_PAREN | RIGHT_PAREN | LEFT_BRACKET | RIGHT_BRACKET
  | ID | STRING | NUMBER
  | EMPTY

type value = STRING of string | FLOAT of float

type token = { lexeme : lexeme; value : value; line : int }
