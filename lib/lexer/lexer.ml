open Token_type

exception Lexing_error of string

let isDigit (c:char) =
  let ascii_code = int_of_char c in
  48 (*0*) <= ascii_code && ascii_code <= 57 (*9*)

let isAlpha (c:char) =
  let ascii_code = int_of_char c in
  (97 <= ascii_code && ascii_code <= 122) ||
  (65 <= ascii_code && ascii_code <= 90) ||
  (ascii_code = 95)

let isAlphaNumeric (c:char) =
  isAlpha c || isDigit c

let string s current line =
  let rec aux value current' line' =
    if current' < 0 then
      raise (Lexing_error "Unfinished string")
    else
      match s.[current'] with
      | '"' -> value, (current'-1), line'
      | '\n' -> aux ("\n"^value) (current'-1) (line'+1)
      | c -> aux (String.make 1 c ^ value) (current'-1) line'
  in
  let value, current', line' = aux "" current line in
  { lexeme = STRING; value = STRING value; line = line }, current', line'

let number s current line =
  let rec aux value current' line' is_float =
    match s.[current'] with
    | '.' when is_float -> raise ( Lexing_error ("[ " ^ string_of_int (Utility.number_of_line s - line') ^ " ] Numbers are declared as <int>.<int> for floats or <int> for integers but not 10.000.000") )
    | '.' -> aux ("." ^ value) (current' - 1) line' true
    | c when isDigit c -> aux (String.make 1 c ^ value) (current' - 1) line' is_float
    | _ -> value, current', line'
  in
  let value, current', line' = aux "" current line false in
  { lexeme = NUMBER; value = FLOAT (float_of_string value); line = line }, current', line'

let id s current line =
  let rec aux value current' line' = 
    if current' >= 0 then
      if isAlphaNumeric s.[current'] then
        aux (String.make 1 s.[current'] ^ value) (current' - 1) line'
      else
        value, current', line'
    else
      value, current', line'
  in
  let value, current', line' = aux (String.make 1 s.[current]) (current-1) line in
  { lexeme = ID; value = STRING value; line = line }, current', line'

let tokenize (s:string) =
  let s = String.trim s in
  let len = String.length s in
  let rec aux res current line =
    if current < 0 then res else
    match s.[current] with
    | '$' -> aux ({ lexeme = DOLLAR; value = STRING "$"; line = line } :: res) (current - 1) line
    | '?' -> aux ({ lexeme = QUESTION_MARK; value = STRING "?"; line = line } :: res) (current - 1) line
    | ':' -> aux ({ lexeme = COLON; value = STRING ":"; line = line } :: res) (current - 1) line
    | '+' -> aux ({ lexeme = PLUS; value = STRING "+"; line = line } :: res) (current - 1) line
    | '!' -> aux ({ lexeme = BANG; value = STRING "!"; line = line } :: res) (current - 1) line
    | '/' -> aux ({ lexeme = SLASH; value = STRING "/"; line = line } :: res) (current - 1) line
    | '(' -> aux ({ lexeme = LEFT_PAREN; value = STRING "("; line = line } :: res) (current - 1) line
    | ')' -> aux ({lexeme = RIGHT_PAREN; value = STRING ")"; line = line } :: res) (current - 1) line
    | '{' -> aux ({ lexeme = LEFT_BRACE; value = STRING "{"; line = line } :: res) (current - 1) line
    | '}' -> aux ({ lexeme = RIGHT_BRACE; value = STRING "}"; line = line } :: res) (current - 1) line
    | '&' ->
      begin
      match s.[ current - 1 ] with
      | '&' -> aux ({ lexeme = D_AMPERSAND; value = STRING "&&"; line = line } :: res) (current-2) line
      | _ -> raise (Lexing_error ("[ " ^ string_of_int (Utility.number_of_line s - line) ^ " ] Unknow character : '&'"))
      end
    | '|' ->
      begin
      match s.[current - 1] with
      | '|' -> aux ({ lexeme = D_VERT_BAR; value = STRING "||"; line = line } :: res) (current - 2) line
      | _ -> raise (Lexing_error ("[ " ^ string_of_int (Utility.number_of_line s - line) ^ " ] Unknow character : '|'"))
      end
    | '>' -> 
      begin
      match s.[ current - 1 ] with
      | '>' -> aux ({ lexeme = D_SUP; value = STRING ">>"; line = line } :: res) (current - 2) line 
      | _ -> aux ({ lexeme = SUP; value = STRING ">"; line = line } :: res) (current - 1) line 
      end
    | '<' ->
      begin
      match s.[ current - 1 ] with
      | '<' -> aux ({ lexeme = D_INF; value = STRING "<<"; line = line } :: res) (current - 2) line
      | '>' -> aux ({ lexeme = SUP_INF; value = STRING "><"; line = line } :: res) (current - 2) line
      | _ -> aux ({ lexeme = INF; value = STRING "<"; line = line } :: res) (current - 1) line
      end
    | '=' ->
      begin
      match s.[ current - 1 ] with
      | '<' -> aux ({ lexeme = INF_EQUAL; value = STRING "<="; line = line } :: res) (current - 2) line
      | '>' -> aux ({ lexeme = SUP_EQUAL; value = STRING ">="; line = line } :: res) (current - 2) line
      | '!' -> aux ({ lexeme = BANG_EQUAL; value = STRING "!="; line = line } :: res) (current - 2) line
      | _ -> aux ({ lexeme = EQUAL; value = STRING "="; line = line } :: res) (current - 1) line
      end
    | '-' -> 
      begin
      match s.[ current - 1 ] with
      | '<' -> aux ({ lexeme = LEFT_ARROW; value = STRING "<-"; line = line } :: res) (current - 2) line 
      | _ -> aux ({ lexeme = MINUS; value = STRING "-"; line = line } :: res) (current - 1) line 
      end
    | '*' -> 
      begin
      match s.[ current - 1 ] with
      | '*' -> aux ({ lexeme = TIME_TIME; value = STRING "**"; line = line } :: res) (current - 2) line 
      | _ -> aux ({ lexeme = TIME; value = STRING "*"; line = line } :: res) (current - 1) line 
      end
    (* | '\n' -> aux ({ lexeme = LINE_BREAK; value = STRING "\\n"; line = line } :: res) (current - 1) (line + 1) *)
    | '\n' -> aux res (current - 1) (line + 1)
    | ' ' | '\t' | '\r' -> aux res (current - 1) line 
    | '"' ->
        let (token, current', line') = string s ( current - 1 ) line in
        aux (token :: res) current' line'
    | c -> 
      if isDigit(c) then
        let (token, current', line') = number s current line in
        aux (token :: res) current' line'
      else if isAlpha(c) then
        let (token, current', line') = id s current line in
        aux (token :: res) current' line'
      else
        raise (Lexing_error ("[ " ^ string_of_int (Utility.number_of_line s - line) ^ " ] Unexpected char : " ^ String.make 1 s.[ current ]))
  in
  aux [] (len-1) 0
