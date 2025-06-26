open Logic_repl

let rec repl counter =
  let prompt = Printf.sprintf "%d # " counter in
  let line_read = LNoise.linenoise prompt in
  match line_read with
  | Some input ->
      let _ = LNoise.history_add input in
      if String.trim input <> "" then (
        try
          input |> (fun s -> Lexer.tokenize s 0) |> Parser.parse |> Evaluate.print_truth_table;
          print_newline ()
        with
        | Parser.Parse_error msg ->
            Printf.printf "Erro de Parsing: %s\n\n" msg;
            flush_all ()
        | e -> 
            Printf.printf "Ocorreu um erro inesperado: %s\n" (Printexc.to_string e);
            flush_all ()
      );
      repl (counter + 1)
  | None -> 
      print_endline "Saindo...";
      ()

let print_header () =
  let width = 80 in
  let line = String.make width '-' in
  Printf.printf "\x1b[36m%s\x1b[0m\n" line;
  Printf.printf "\x1b[36m<\x1b[0m \x1b[33mLogic REPL\x1b[0m \x1b[36m>\x1b[0m\n";
  Printf.printf "\x1b[36m%s\x1b[0m\n" line;
  print_newline ();
  
  Printf.printf "\x1b[32mTokens disponíveis:\x1b[0m\n";
  Printf.printf "  \x1b[33mOperadores:\x1b[0m \x1b[36m+\x1b[0m (OR)  \x1b[36m.\x1b[0m (AND)  \x1b[36m!\x1b[0m (NOT)  \x1b[36m^\x1b[0m (XOR)\n";
  Printf.printf "  \x1b[33mImplicações:\x1b[0m \x1b[36m->\x1b[0m (IF)  \x1b[36m<->\x1b[0m (IFF)\n";
  Printf.printf "  \x1b[33mEspeciais:\x1b[0m \x1b[36m!&\x1b[0m (NAND)  \x1b[36m!|\x1b[0m (NOR)\n";
  Printf.printf "  \x1b[33mConstantes:\x1b[0m \x1b[36mTRUE\x1b[0m  \x1b[36mFALSE\x1b[0m\n";
  Printf.printf "  \x1b[33mVariáveis:\x1b[0m \x1b[36mp\x1b[0m, \x1b[36mq\x1b[0m, \x1b[36mr\x1b[0m, etc.\n";
  Printf.printf "  \x1b[33mParênteses:\x1b[0m \x1b[36m(\x1b[0m  \x1b[36m)\x1b[0m\n";
  print_newline ();
  
  Printf.printf "\x1b[32mExemplos:\x1b[0m\n";
  Printf.printf "  \x1b[36mp + q\x1b[0m         → p OR q\n";
  Printf.printf "  \x1b[36m!(p . q)\x1b[0m      → NOT(p AND q)\n";
  Printf.printf "  \x1b[36mp -> q\x1b[0m        → p implica q\n";
  Printf.printf "  \x1b[36m(p ^ q) <-> r\x1b[0m → (p XOR q) se e somente se r\n";
  print_newline ();
  
  Printf.printf "\x1b[32mInstruções:\x1b[0m\n";
  Printf.printf "  • Digite uma expressão e pressione \x1b[33mEnter\x1b[0m para avaliar\n";
  Printf.printf "  • Use \x1b[33mCtrl+C\x1b[0m para sair\n";
  Printf.printf "  • Setas ↑↓ para navegar no histórico\n";
  print_newline ();
  
  Printf.printf "\x1b[36m%s\x1b[0m\n" line;
  print_newline ()

let () =
  print_header ();
  
  LNoise.set_hints_callback (fun _ -> None);
  LNoise.set_completion_callback (fun _line_so_far completions ->
    let suggestions = ["and"; "or"; "not"; "true"; "false"] in
    List.iter (fun s -> LNoise.add_completion completions s) suggestions
  );
  
  repl 1
