let test_suites: unit Alcotest.test list = [
  "JackTokenizer", JackTokenizer.tests;
  "CompilerEngine", CompilerEngine.tests;
]

let () = Alcotest.run "proj" test_suites
