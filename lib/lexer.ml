open Token

let is_letter c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let rec tokenize input pos =
  if pos >= String.length input then
    []
  else
    match input.[pos] with
    | ' ' | '\t' | '\n' -> tokenize input (pos + 1)
    | '-' ->
        if pos + 1 < String.length input && input.[pos + 1] = '>' then
          IF :: tokenize input (pos + 2)
        else
          [ILLEGAL ("-", pos)]

    | '<' ->
        if pos + 2 < String.length input && String.sub input pos 3 = "<->" then
          IFF :: tokenize input (pos + 3)
        else
          [ILLEGAL ("<", pos)]

    | '!' ->
        if pos + 1 < String.length input then
          begin match input.[pos + 1] with
          | '&' -> NAND :: tokenize input (pos + 2)
          | '|' -> NOR :: tokenize input (pos + 2)
          | _ -> NOT :: tokenize input (pos + 1)
          end
        else
          NOT :: tokenize input (pos + 1)

    | '+' -> OR :: tokenize input (pos + 1)
    | '.' -> AND :: tokenize input (pos + 1)
    | '^' -> XOR :: tokenize input (pos + 1)
    | '(' -> LPAREN :: tokenize input (pos + 1)
    | ')' -> RPAREN :: tokenize input (pos + 1)

    | c when is_letter c ->
        let start = pos in
        let rec find_end i =
          if i < String.length input && is_letter input.[i] then
            find_end (i + 1)
          else i
        in
        let end_pos = find_end pos in
        let name = String.sub input start (end_pos - start) in
        let tok =
          match String.lowercase_ascii name with
          | "true" -> TRUE
          | "false" -> FALSE
          | _ -> VAR name
        in
        tok :: tokenize input end_pos

    | c -> [ILLEGAL (String.make 1 c, pos)]
