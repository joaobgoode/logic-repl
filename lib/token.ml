type token =
  | TRUE
  | FALSE
  | OR
  | AND
  | NOT
  | VAR of string
  | XOR
  | IF
  | IFF
  | LPAREN
  | RPAREN
  | NAND
  | NOR
  | ILLEGAL of string * int

let string_of_token = function
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | OR -> "+"
  | AND -> "."
  | NOT -> "!"
  | VAR s -> s
  | XOR -> "^"
  | IF -> "->"
  | IFF -> "<->"
  | LPAREN -> "("
  | RPAREN -> ")"
  | NAND -> "!&"
  | NOR -> "!|"
  | ILLEGAL (s, pos) -> Printf.sprintf "ILLEGAL(%s at %d)" s pos

