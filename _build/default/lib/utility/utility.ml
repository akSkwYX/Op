let number_of_line s =
  String.fold_left (fun acc c -> if c == '\n' then acc + 1 else acc) 0 s
