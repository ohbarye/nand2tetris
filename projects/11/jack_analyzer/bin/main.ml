open Nand2tetris_compiler

let main =
  JackAnalyzer.run Sys.argv.(1)

let () = main
