(lang dune 3.19)
(name logic-repl)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Joao Victor Vieira Marcondes")

(maintainers "Joao Victor Vieira Marcondes")

(license LICENSE)


(package
 (name logic-repl)
 (synopsis "REPL para Logica Proposicional")
 (description "Um REPL para Lógica Proposicional é uma interface interativa de linha de comando onde o usuário pode digitar expressões lógicas, avaliá-las e ver seus resultados em tempo real. Ele permite testar fórmulas, verificar valores de verdade com diferentes interpretações e explorar operadores como . (e), + (ou), ! (não), -> (implica), <-> (se e somente se) de forma dinâmica e intuitiva. Ideal para aprendizado, testes rápidos e experimentação com lógica proposicional.")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

; 
