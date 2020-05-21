module To_test = struct
  let tokenize_unit = Nand2tetris_compiler.JackTokenizer.tokenize_unit
  let tokenize_line = Nand2tetris_compiler.JackTokenizer.tokenize_line
  let tokenize = Nand2tetris_compiler.JackTokenizer.tokenize
end

let check_list input result f =
  Alcotest.(check (list string)) "same list" result (f input [])

let test_tokenize_unit () =
  let f = To_test.tokenize_unit in
  check_list "function" ["function"] f;
  check_list "main()" ["main"; "("; ")"] f

let test_tokenize_line () =
  let f = To_test.tokenize_line in
  check_list "class Main {" ["class"; "Main"; "{"] f;
  check_list "function void main() {" ["function"; "void"; "main"; "("; ")"; "{"] f

let test_whole_tokenize content expected =
  Alcotest.(check (list string)) "same list" expected (To_test.tokenize content)

let test_tokenize_ArrayTest () =
  test_whole_tokenize {eos|
// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/10/ArrayTest/Main.jack

// (identical to projects/09/Average/Main.jack)

/** Computes the average of a sequence of integers. */
class Main {
    function void main() {
        var Array a;
        var int length;
        var int i, sum;

	let length = Keyboard.readInt("HOW MANY NUMBERS? ");
	let a = Array.new(length);
	let i = 0;

	while (i < length) {
	    let a[i] = Keyboard.readInt("ENTER THE NEXT NUMBER: ");
	    let i = i + 1;
	}

	let i = 0;
	let sum = 0;

	while (i < length) {
	    let sum = sum + a[i];
	    let i = i + 1;
	}

	do Output.printString("THE AVERAGE IS: ");
	do Output.printInt(sum / length);
	do Output.println();

	return;
    }
}
      |eos} ["class"; "Main"; "{"; "function"; "void"; "main"; "("; ")"; "{"; "var"; "Array"; "a"; ";"; "var"; "int"; "length"; ";"; "var"; "int"; "i"; ","; "sum"; ";"; "let"; "length"; "="; "Keyboard"; "."; "readInt"; "("; "\"HOW MANY NUMBERS? \""; ")"; ";"; "let"; "a"; "="; "Array"; "."; "new"; "("; "length"; ")"; ";"; "let"; "i"; "="; "0"; ";"; "while"; "("; "i"; "<"; "length"; ")"; "{"; "let"; "a"; "["; "i"; "]"; "="; "Keyboard"; "."; "readInt"; "("; "\"ENTER THE NEXT NUMBER: \""; ")"; ";"; "let"; "i"; "="; "i"; "+"; "1"; ";"; "}"; "let"; "i"; "="; "0"; ";"; "let"; "sum"; "="; "0"; ";"; "while"; "("; "i"; "<"; "length"; ")"; "{"; "let"; "sum"; "="; "sum"; "+"; "a"; "["; "i"; "]"; ";"; "let"; "i"; "="; "i"; "+"; "1"; ";"; "}"; "do"; "Output"; "."; "printString"; "("; "\"THE AVERAGE IS: \""; ")"; ";"; "do"; "Output"; "."; "printInt"; "("; "sum"; "/"; "length"; ")"; ";"; "do"; "Output"; "."; "println"; "("; ")"; ";"; "return"; ";"; "}"; "}"; ]

  let test_tokenize_ExpressionLessSquare () =
    test_whole_tokenize {eos|
      |eos} []

  let test_tokenize_Square () =
    test_whole_tokenize {eos|
      |eos} []


let tests = [
  "test_tokenize_unit", `Quick, test_tokenize_unit;
  "test_tokenize_line", `Quick, test_tokenize_line;
  "test_tokenize_ArrayTest", `Quick, test_tokenize_ArrayTest;
  "test_tokenize_ExpressionLessSquare", `Quick, test_tokenize_ExpressionLessSquare;
  "test_tokenize_Square", `Quick, test_tokenize_Square;
]