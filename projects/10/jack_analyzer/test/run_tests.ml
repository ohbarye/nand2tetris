let test_suites: unit Alcotest.test list = [
  "JackTokenizer", JackTokenizer.tests;
]

let () = Alcotest.run "proj" test_suites
