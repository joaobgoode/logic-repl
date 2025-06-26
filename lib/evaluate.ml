open Ast
module StringSet = Set.Make(String)

let collect_vars expr =
  let rec aux acc = function
    | Bool _ -> acc
    | Var s -> StringSet.add s acc
    | Not e -> aux acc e
    | And (a, b) | Or (a, b) | Xor (a, b)
    | Nand (a, b) | Nor (a, b) | If (a, b) | Iff (a, b) ->
        let acc = aux acc a in
        aux acc b
  in
  StringSet.elements (aux StringSet.empty expr)

let generate_envs vars =
  let rec aux = function
    | [] -> [ [] ]
    | v :: vs ->
        let rest = aux vs in
        List.concat [
          List.map (fun env -> (v, true) :: env) rest;
          List.map (fun env -> (v, false) :: env) rest;
        ]
  in
  let env_lists = aux vars in
  List.map
    (fun lst -> fun var -> List.assoc var lst)
    env_lists

let rec eval expr env =
  match expr with
  | Bool b -> b
  | Var s -> env s
  | Not e -> not (eval e env)
  | And (a, b) -> eval a env && eval b env
  | Or (a, b) -> eval a env || eval b env
  | Xor (a, b) -> eval a env <> eval b env
  | Nand (a, b) -> not (eval a env && eval b env)
  | Nor (a, b) -> not (eval a env || eval b env)
  | If (a, b) -> not (eval a env) || eval b env
  | Iff (a, b) -> eval a env = eval b env

let[@coverage off] print_truth_table expr =
  let vars = collect_vars expr in
  let envs = generate_envs vars in

  List.iter (fun v -> Printf.printf "%s\t" v) vars;
  Printf.printf "| Resultado\n";
  Printf.printf "%s\n" (String.make ((List.length vars * 8) + 10) '-');

  List.iter (fun env ->
    List.iter (fun v -> Printf.printf "%B\t" (env v)) vars;
    Printf.printf "| %B\n" (eval expr env)
  ) envs

