open Logic_repl

let run_test name test_func =
  Printf.printf "Running test: %s... " name;
  if test_func () then
    Printf.printf "PASS\n"
  else
    Printf.printf "FAIL\n"

let compare_token_lists_for_test l1 l2 =
  let normalize_token t =
    match t with
    | Token.ILLEGAL (s, _) -> Token.ILLEGAL (s, 0) 
    | other -> other
  in
  (List.map normalize_token l1) = (List.map normalize_token l2)

let test_lexer_empty_string () =
  let tokens = Lexer.tokenize "" 0 in
  tokens = []

let test_lexer_whitespace_only () =
  let tokens = Lexer.tokenize " \t\n " 0 in
  tokens = []

let test_lexer_simple_vars_and_bools () =
  let tokens = Lexer.tokenize "P true Q false" 0 in
  compare_token_lists_for_test tokens [VAR "P"; TRUE; VAR "Q"; FALSE]

let test_lexer_all_operators () =
  let tokens = Lexer.tokenize "().+!^-><->!&!|" 0 in
  compare_token_lists_for_test tokens [
    LPAREN; RPAREN; AND; OR; NOT; XOR; IF; IFF; NAND; NOR
  ]

let test_lexer_mixed_expression () =
  let tokens = Lexer.tokenize "(!A + (B -> C)) <-> false" 0 in
  compare_token_lists_for_test tokens [
    LPAREN; NOT; VAR "A"; OR; LPAREN; VAR "B"; IF; VAR "C"; RPAREN; RPAREN; IFF; FALSE
  ]

let test_lexer_illegal_char () =
  let tokens = Lexer.tokenize "A # B" 0 in
  compare_token_lists_for_test tokens [VAR "A"; ILLEGAL ("#", 2)]

let test_lexer_partial_operator () =
  let tokens = Lexer.tokenize "A -" 0 in
  compare_token_lists_for_test tokens [VAR "A"; ILLEGAL ("-", 2)]


let test_lexer_partial_operator2 () =
  let tokens = Lexer.tokenize "A < B" 0 in
  compare_token_lists_for_test tokens [VAR "A"; ILLEGAL ("<", 2)]

let test_lexer_not_operator () =
  let tokens = Lexer.tokenize "A!" 0 in
  compare_token_lists_for_test tokens [VAR "A"; NOT]

let () =
  Printf.printf "\n--- Running Lexer tests ---\n";
  run_test "Empty string" test_lexer_empty_string;
  run_test "Whitespace only" test_lexer_whitespace_only;
  run_test "Simple variables and bools" test_lexer_simple_vars_and_bools;
  run_test "All operators" test_lexer_all_operators;
  run_test "Mixed expression" test_lexer_mixed_expression;
  run_test "Illegal character" test_lexer_illegal_char;
  run_test "Partial operator" test_lexer_partial_operator;
  run_test "Partial operator" test_lexer_partial_operator2;
  run_test "Not operator" test_lexer_not_operator;
  Printf.printf "--- Lexer tests finished ---\n"
