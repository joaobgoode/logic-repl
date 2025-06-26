open Logic_repl

let run_test name test_func =
  Printf.printf "Running test: %s... " name;
  if test_func () then
    Printf.printf "PASS\n"
  else
    Printf.printf "FAIL\n"

let env_from_list (lst : (string * bool) list) (var : string) : bool =
  List.assoc var lst

let compare_string_lists lst1 lst2 =
  let sort_and_deduplicate l =
    List.sort String.compare (List.sort_uniq String.compare l)
  in
  (sort_and_deduplicate lst1) = (sort_and_deduplicate lst2)

let env_to_list (env_func : string -> bool) (vars : string list) : (string * bool) list =
  List.map (fun v -> (v, env_func v)) vars

let sort_env_representation (env_list : (string * bool) list) =
  List.sort (fun (s1, _) (s2, _) -> String.compare s1 s2) env_list

let parse_string_to_ast s =
  let tokens = Lexer.tokenize s 0 in
  Parser.parse tokens


let test_collect_vars_no_vars_str () =
  let expr = parse_string_to_ast "true" in
  let vars = Evaluate.collect_vars expr in
  vars = []

let test_collect_vars_single_var_str () =
  let expr = parse_string_to_ast "P" in
  let vars = Evaluate.collect_vars expr in
  compare_string_lists vars ["P"]

let test_collect_vars_multiple_unique_vars_str () =
  let expr = parse_string_to_ast "A . (B + C)" in
  let vars = Evaluate.collect_vars expr in
  compare_string_lists vars ["A"; "B"; "C"]

let test_collect_vars_repeated_vars_str () =
  let expr = parse_string_to_ast "X . X" in
  let vars = Evaluate.collect_vars expr in
  compare_string_lists vars ["X"]

let test_collect_vars_nested_and_mixed_str () =
  let expr = parse_string_to_ast "!(A) -> (B <-> (true . C))" in
  let vars = Evaluate.collect_vars expr in
  compare_string_lists vars ["A"; "B"; "C"]

let test_generate_envs_zero_vars () =
  let vars = [] in
  let envs = Evaluate.generate_envs vars in
  List.length envs = 1 &&
  (let actual_env_list = List.map (fun f -> env_to_list f vars) envs in
   actual_env_list = [[]])

let test_generate_envs_one_var () =
  let vars = ["P"] in
  let envs = Evaluate.generate_envs vars in
  let expected_count = 2 in
  let actual_count = List.length envs in
  let expected_env_representations =
    [ [("P", true)]; [("P", false)] ]
  in
  let actual_env_representations =
    List.map (fun f -> env_to_list f vars) envs
    |> List.map sort_env_representation
    |> List.sort compare
  in
  let sorted_expected =
    List.map sort_env_representation expected_env_representations
    |> List.sort compare
  in
  actual_count = expected_count && actual_env_representations = sorted_expected

let test_generate_envs_two_vars () =
  let vars = ["A"; "B"] in
  let envs = Evaluate.generate_envs vars in
  let expected_count = 4 in
  let actual_count = List.length envs in
  let expected_env_representations =
    [ [("A", true); ("B", true)];
      [("A", true); ("B", false)];
      [("A", false); ("B", true)];
      [("A", false); ("B", false)] ]
  in
  let actual_env_representations =
    List.map (fun f -> env_to_list f vars) envs
    |> List.map sort_env_representation
    |> List.sort compare
  in
  let sorted_expected =
    List.map sort_env_representation expected_env_representations
    |> List.sort compare
  in
  actual_count = expected_count && actual_env_representations = sorted_expected

let test_eval_bool_true_str () =
  let expr = parse_string_to_ast "true" in
  let env = env_from_list [] in
  Evaluate.eval expr env = true

let test_eval_bool_false_str () =
  let expr = parse_string_to_ast "false" in
  let env = env_from_list [] in
  Evaluate.eval expr env = false

let test_eval_var_true_str () =
  let expr = parse_string_to_ast "X" in
  let env = env_from_list [("X", true)] in
  Evaluate.eval expr env = true

let test_eval_var_false_str () =
  let expr = parse_string_to_ast "Y" in
  let env = env_from_list [("Y", false)] in
  Evaluate.eval expr env = false

let test_eval_not_true_str () =
  let expr = parse_string_to_ast "!true" in
  let env = env_from_list [] in
  Evaluate.eval expr env = false

let test_eval_not_false_str () =
  let expr = parse_string_to_ast "!false" in
  let env = env_from_list [] in
  Evaluate.eval expr env = true

let test_eval_and_tt_str () =
  let expr = parse_string_to_ast "true . true" in
  let env = env_from_list [] in
  Evaluate.eval expr env = true

let test_eval_and_tf_str () =
  let expr = parse_string_to_ast "true . false" in
  let env = env_from_list [] in
  Evaluate.eval expr env = false

let test_eval_or_tf_str () =
  let expr = parse_string_to_ast "true + false" in
  let env = env_from_list [] in
  Evaluate.eval expr env = true

let test_eval_xor_tt_str () =
  let expr = parse_string_to_ast "true ^ true" in
  let env = env_from_list [] in
  Evaluate.eval expr env = false

let test_eval_xor_tf_str () =
  let expr = parse_string_to_ast "true ^ false" in
  let env = env_from_list [] in
  Evaluate.eval expr env = true

let test_eval_nand_tt_str () =
  let expr = parse_string_to_ast "true !& true" in
  let env = env_from_list [] in
  Evaluate.eval expr env = false

let test_eval_nand_ff_str () =
  let expr = parse_string_to_ast "false !& false" in
  let env = env_from_list [] in
  Evaluate.eval expr env = true

let test_eval_nor_ff_str () =
  let expr = parse_string_to_ast "false !| false" in
  let env = env_from_list [] in
  Evaluate.eval expr env = true

let test_eval_if_ft_str () =
  let expr = parse_string_to_ast "false -> true" in
  let env = env_from_list [] in
  Evaluate.eval expr env = true

let test_eval_if_tf_str () =
  let expr = parse_string_to_ast "true -> false" in
  let env = env_from_list [] in
  Evaluate.eval expr env = false

let test_eval_iff_tt_str () =
  let expr = parse_string_to_ast "true <-> true" in
  let env = env_from_list [] in
  Evaluate.eval expr env = true

let test_eval_iff_tf_str () =
  let expr = parse_string_to_ast "true <-> false" in
  let env = env_from_list [] in
  Evaluate.eval expr env = false

let test_eval_complex_expr_tt_f_str () =
  let expr = parse_string_to_ast "(A . B) + (!C)" in
  let env = env_from_list [("A", true); ("B", true); ("C", false)] in
  Evaluate.eval expr env = true 

let test_eval_complex_expr_tf_t_str () =
  let expr = parse_string_to_ast "(A . B) + (!C)" in
  let env = env_from_list [("A", true); ("B", false); ("C", true)] in
  Evaluate.eval expr env = false 

let test_eval_rparent_test () =
  try 
    let expr = parse_string_to_ast "(A . B) + (!C" in
    let env = env_from_list [("A", true); ("B", false); ("C", true)] in
    Evaluate.eval expr env
  with
    | Parser.Parse_error "Esperado RPAREN" -> true
    | _ -> false

let test_eval_ilegal_token () =
  try 
    let expr = parse_string_to_ast "(A . B) + (!C*)" in
    let env = env_from_list [("A", true); ("B", false); ("C", true)] in
    Evaluate.eval expr env
  with
    | Parser.Parse_error "Token ilegal '*' na posição 13" -> true
    | _ -> false

let test_eval_remaining_tokens () =
  try 
    let expr = parse_string_to_ast "(A . B) + !C)" in
    let env = env_from_list [("A", true); ("B", false); ("C", true)] in
    Evaluate.eval expr env
  with
    | Parser.Parse_error "Tokens restantes após parsing" -> true
    | _ -> false

let test_eval_unnexpected_tokens () =
  try 
    let expr = parse_string_to_ast "+(A . B) + !C)" in
    let env = env_from_list [("A", true); ("B", false); ("C", true)] in
    Evaluate.eval expr env
  with
    | Parser.Parse_error "Token inesperado" -> true
    | _ -> false

let test_eval_unnexpected_tokens2 () =
  try 
    let expr = parse_string_to_ast ")(A . B) + !C)" in
    let env = env_from_list [("A", true); ("B", false); ("C", true)] in
    Evaluate.eval expr env
  with
    | Parser.Parse_error "Token inesperado" -> true
    | _ -> false

let test_eval_unnexpected_tokens3 () =
  try 
    let expr = parse_string_to_ast "" in
    let env = env_from_list [] in
    Evaluate.eval expr env
  with
    | Parser.Parse_error "Token inesperado" -> true
    | _ -> false

let test_eval_unnexpected_tokens4 () =
  try 
    let expr = parse_string_to_ast "(!)" in
    let env = env_from_list [] in
    Evaluate.eval expr env
  with
    | Parser.Parse_error "Token inesperado" -> true
    | _ -> false


let () =
  Printf.printf "\n--- Running all Evaluate module tests ---\n";

  Printf.printf "\n--- collect_vars tests (from string) ---\n";
  run_test "No variables (string)" test_collect_vars_no_vars_str;
  run_test "Single variable (string)" test_collect_vars_single_var_str;
  run_test "Multiple unique variables (string)" test_collect_vars_multiple_unique_vars_str;
  run_test "Repeated variables (string)" test_collect_vars_repeated_vars_str;
  run_test "Nested and mixed expressions (string)" test_collect_vars_nested_and_mixed_str;

  Printf.printf "\n--- generate_envs tests (unchanged) ---\n";
  run_test "Zero variables" test_generate_envs_zero_vars;
  run_test "One variable" test_generate_envs_one_var;
  run_test "Two variables" test_generate_envs_two_vars;

  Printf.printf "\n--- eval tests (from string) ---\n";
  run_test "eval Bool true (string)" test_eval_bool_true_str;
  run_test "eval Bool false (string)" test_eval_bool_false_str;
  run_test "eval Var true (string)" test_eval_var_true_str;
  run_test "eval Var false (string)" test_eval_var_false_str;
  run_test "eval Not true (string)" test_eval_not_true_str;
  run_test "eval Not false (string)" test_eval_not_false_str;
  run_test "eval And true true (string)" test_eval_and_tt_str;
  run_test "eval And true false (string)" test_eval_and_tf_str;
  run_test "eval Or true false (string)" test_eval_or_tf_str;
  run_test "eval Xor true true (string)" test_eval_xor_tt_str;
  run_test "eval Xor true false (string)" test_eval_xor_tf_str;
  run_test "eval Nand true true (string)" test_eval_nand_tt_str;
  run_test "eval Nand true false (string)" test_eval_nand_ff_str;
  run_test "eval Nor false false (string)" test_eval_nor_ff_str;
  run_test "eval If false true (string)" test_eval_if_ft_str;
  run_test "eval If true false (string)" test_eval_if_tf_str;
  run_test "eval Iff true true (string)" test_eval_iff_tt_str;
  run_test "eval Iff true false (string)" test_eval_iff_tf_str;
  run_test "eval Complex Expr 1 (string)" test_eval_complex_expr_tt_f_str;
  run_test "eval Complex Expr 2 (string)" test_eval_complex_expr_tf_t_str;
  run_test "eval Missing Closing Paren (string)" test_eval_rparent_test;
  run_test "eval Illegal Token (string)" test_eval_ilegal_token;
  run_test "eval Remaining Tokens (string)" test_eval_remaining_tokens;
  run_test "eval Unexpected Tokens (string)" test_eval_unnexpected_tokens;
  run_test "eval Unexpected Tokens (string)" test_eval_unnexpected_tokens2;
  run_test "eval Unexpected Tokens (string)" test_eval_unnexpected_tokens3;
  run_test "eval Unexpected Tokens (string)" test_eval_unnexpected_tokens4;

  Printf.printf "\n--- All Evaluate module tests finished ---\n"
