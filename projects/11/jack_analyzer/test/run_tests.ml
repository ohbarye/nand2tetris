let test_suites: unit Alcotest.test list = [
  (* "JackTokenizer", JackTokenizer.tests;
     "CompilerEngine", CompilerEngine.tests; *)
  "SymbolTable", SymbolTable.tests;
]

let () = Alcotest.run "proj" test_suites
