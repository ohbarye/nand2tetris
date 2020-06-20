module To_test = struct
  let compile = Nand2tetris_compiler.CompilerEngine.compile
end

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  Batteries.String.trim s

let compare impl original =
  let rec comp = function
    | ([], []) -> (); (* success *)
    | (h1 :: tl1, h2 :: tl2) ->
      Alcotest.(check string) "same string" (Batteries.String.trim h1) (Batteries.String.trim h2);
      comp (tl1, tl2)
    | _ ->
      Alcotest.(check bool) "Fail due to unmatch between result and expected" true false in
  let result = Batteries.String.split_on_string (read_whole_file impl) ~by: "\n" in
  let expected = Batteries.String.split_on_string (read_whole_file original) ~by: "\n" in
  comp (result, expected)

let test_compile_ArrayTest_Main () =
  To_test.compile "../ArrayTest/Main.jack";
  compare "../ArrayTest/Main.impl.xml" "../ArrayTest/Main.xml"

let test_compile_ExpressionLessSquare_Main () =
  To_test.compile "../ExpressionLessSquare/Main.jack";
  compare "../ExpressionLessSquare/Main.impl.xml" "../ExpressionLessSquare/Main.xml"

let test_compile_ExpressionLessSquare_Square () =
  To_test.compile "../ExpressionLessSquare/Square.jack";
  compare "../ExpressionLessSquare/Square.impl.xml" "../ExpressionLessSquare/Square.xml"

let test_compile_ExpressionLessSquare_SquareGame () =
  To_test.compile "../ExpressionLessSquare/SquareGame.jack";
  compare "../ExpressionLessSquare/SquareGame.impl.xml" "../ExpressionLessSquare/SquareGame.xml"

let test_compile_Square_Main () =
  To_test.compile "../Square/Main.jack";
  compare "../Square/Main.impl.xml" "../Square/Main.xml"

let test_compile_Square_Square () =
  To_test.compile "../Square/Square.jack";
  compare "../Square/Square.impl.xml" "../Square/Square.xml"

let test_compile_Square_SquareGame () =
  To_test.compile "../Square/SquareGame.jack";
  compare "../Square/SquareGame.impl.xml" "../Square/SquareGame.xml"

let tests = [
  "test_compile_ArrayTest_Main", `Quick, test_compile_ArrayTest_Main;
  "test_compile_ExpressionLessSquare_Main", `Quick, test_compile_ExpressionLessSquare_Main;
  "test_compile_ExpressionLessSquare_Square", `Quick, test_compile_ExpressionLessSquare_Square;
  "test_compile_ExpressionLessSquare_SquareGame", `Quick, test_compile_ExpressionLessSquare_SquareGame;
  "test_compile_Square_Main", `Quick, test_compile_Square_Main;
  "test_compile_Square_Square", `Quick, test_compile_Square_Square;
  "test_compile_Square_SquareGame", `Quick, test_compile_Square_SquareGame;
]
