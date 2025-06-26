open Token
open Ast

exception Parse_error of string

type token_stream = token list

let rec parse_expr tokens =
  let left, rest = parse_if tokens in
  match rest with
  | IFF :: tl ->
      let right, rest' = parse_expr tl in
      (Iff (left, right), rest')
  | _ -> (left, rest)

and parse_if tokens =
  let left, rest = parse_or tokens in
  match rest with
  | IF :: tl ->
      let right, rest' = parse_if tl in
      (If (left, right), rest')
  | _ -> (left, rest)

and parse_or tokens =
  let left, rest = parse_and tokens in
  match rest with
  | OR :: tl ->
      let right, rest' = parse_or tl in
      (Or (left, right), rest')
  | NOR :: tl ->
      let right, rest' = parse_or tl in
      (Nor (left, right), rest')
  | XOR :: tl ->
      let right, rest' = parse_or tl in
      (Xor (left, right), rest')
  | _ -> (left, rest)

and parse_and tokens =
  let left, rest = parse_not tokens in
  match rest with
  | AND :: tl ->
      let right, rest' = parse_and tl in
      (And (left, right), rest')
  | NAND :: tl ->
      let right, rest' = parse_and tl in
      (Nand (left, right), rest')
  | _ -> (left, rest)

and parse_not tokens =
  match tokens with
  | NOT :: tl ->
      let expr, rest = parse_not tl in
      (Not expr, rest)
  | _ -> parse_atom tokens

and parse_atom tokens =
  match tokens with
  | TRUE :: tl -> (Bool true, tl)
  | FALSE :: tl -> (Bool false, tl)
  | VAR s :: tl -> (Var s, tl)
  | LPAREN :: tl ->
      let expr, rest = parse_expr tl in
      (match rest with
        | RPAREN :: tl' -> (expr, tl')
        | _ -> raise (Parse_error "Esperado RPAREN"))
  | _ -> raise (Parse_error "Token inesperado")

let parse tokens =
  List.iter (fun t ->
    match t with
    | ILLEGAL (sym, pos) -> raise (Parse_error (Printf.sprintf "Token ilegal '%s' na posição %d" sym pos))
    | _ -> ()
  ) tokens;
  let expr, rest = parse_expr tokens in
  match rest with
  | [] -> expr
  | _ -> raise (Parse_error "Tokens restantes após parsing")
