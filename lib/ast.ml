type expr =
  | Bool of bool
  | Var of string
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Xor of expr * expr
  | Nand of expr * expr
  | Nor of expr * expr
  | If of expr * expr
  | Iff of expr * expr

let rec expr_to_string expr =
  match expr with
  | Bool true -> "TRUE"
  | Bool false -> "FALSE"
  | Var x -> x
  | Not e -> "!" ^ wrap e
  | And (a, b) -> "(" ^ expr_to_string a ^ " . " ^ expr_to_string b ^ ")"
  | Or (a, b) -> "(" ^ expr_to_string a ^ " + " ^ expr_to_string b ^ ")"
  | Xor (a, b) -> "(" ^ expr_to_string a ^ " ^ " ^ expr_to_string b ^ ")"
  | Nand (a, b) -> "(" ^ expr_to_string a ^ " ↑ " ^ expr_to_string b ^ ")"
  | Nor (a, b) -> "(" ^ expr_to_string a ^ " ↓ " ^ expr_to_string b ^ ")"
  | If (a, b) -> "(" ^ expr_to_string a ^ " -> " ^ expr_to_string b ^ ")"
  | Iff (a, b) -> "(" ^ expr_to_string a ^ " <-> " ^ expr_to_string b ^ ")"

and wrap expr =
  match expr with
  | Var _ | Bool _ -> expr_to_string expr
  | _ -> "(" ^ expr_to_string expr ^ ")"
