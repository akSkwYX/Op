type lexeme =
  ID | STRING | NUMBER
  | LEFT_ARROW
  | QUESTION_MARK | COLON | TIME_TIME | DOLLAR | LEFT_BRACE | RIGHT_BRACE
  | D_VERT_BAR | D_AMPERSAND | BANG_EQUAL | EQUAL
  | SUP | SUP_EQUAL | INF | INF_EQUAL
  | PLUS | MINUS | SLASH | TIME
  | BANG
  | D_SUP | D_INF | SUP_INF
  | LEFT_PAREN | RIGHT_PAREN
  | EMPTY

type value = STRING of string | FLOAT of float

type token = { lexeme : lexeme; value : value; line : int }
