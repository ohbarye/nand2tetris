module To_test = struct
  let dest = Nand2tetris_assembler.Code.dest
  let comp = Nand2tetris_assembler.Code.comp
  let jump = Nand2tetris_assembler.Code.jump
end

let test_dest () =
  Alcotest.(check string) "same string" "000" (To_test.dest None);
  Alcotest.(check string) "same string" "001" (To_test.dest (Some "M"));
  Alcotest.(check string) "same string" "010" (To_test.dest (Some "D"));
  Alcotest.(check string) "same string" "011" (To_test.dest (Some "MD"));
  Alcotest.(check string) "same string" "100" (To_test.dest (Some "A"));
  Alcotest.(check string) "same string" "101" (To_test.dest (Some "AM"));
  Alcotest.(check string) "same string" "110" (To_test.dest (Some "AD"));
  Alcotest.(check string) "same string" "111" (To_test.dest (Some "AMD"))

let test_comp () =
  Alcotest.(check string) "same string" "0101010" (To_test.comp "0");
  Alcotest.(check string) "same string" "0111111" (To_test.comp "1");
  Alcotest.(check string) "same string" "0111010" (To_test.comp "-1");
  Alcotest.(check string) "same string" "0001100" (To_test.comp "D");
  Alcotest.(check string) "same string" "0110000" (To_test.comp "A");
  Alcotest.(check string) "same string" "0001101" (To_test.comp "!D");
  Alcotest.(check string) "same string" "0110001" (To_test.comp "!A");
  Alcotest.(check string) "same string" "0001111" (To_test.comp "-D");
  Alcotest.(check string) "same string" "0110011" (To_test.comp "-A");
  Alcotest.(check string) "same string" "0011111" (To_test.comp "D+1");
  Alcotest.(check string) "same string" "0110111" (To_test.comp "A+1");
  Alcotest.(check string) "same string" "0001110" (To_test.comp "D-1");
  Alcotest.(check string) "same string" "0110010" (To_test.comp "A-1");
  Alcotest.(check string) "same string" "0000010" (To_test.comp "D+A");
  Alcotest.(check string) "same string" "0010011" (To_test.comp "D-A");
  Alcotest.(check string) "same string" "0000111" (To_test.comp "A-D");
  Alcotest.(check string) "same string" "0000000" (To_test.comp "D&A");
  Alcotest.(check string) "same string" "0010101" (To_test.comp "D|A");
  Alcotest.(check string) "same string" "1110000" (To_test.comp "M");
  Alcotest.(check string) "same string" "1110001" (To_test.comp "!M");
  Alcotest.(check string) "same string" "1110011" (To_test.comp "-M");
  Alcotest.(check string) "same string" "1110111" (To_test.comp "M+1");
  Alcotest.(check string) "same string" "1110010" (To_test.comp "M-1");
  Alcotest.(check string) "same string" "1000010" (To_test.comp "D+M");
  Alcotest.(check string) "same string" "1010011" (To_test.comp "D-M");
  Alcotest.(check string) "same string" "1000111" (To_test.comp "M-D");
  Alcotest.(check string) "same string" "1000000" (To_test.comp "D&M");
  Alcotest.(check string) "same string" "1010101" (To_test.comp "D|M")

let test_jump () =
  Alcotest.(check string) "same string" "000" (To_test.jump None);
  Alcotest.(check string) "same string" "001" (To_test.jump (Some "JGT"));
  Alcotest.(check string) "same string" "010" (To_test.jump (Some "JEQ"));
  Alcotest.(check string) "same string" "011" (To_test.jump (Some "JGE"));
  Alcotest.(check string) "same string" "100" (To_test.jump (Some "JLT"));
  Alcotest.(check string) "same string" "101" (To_test.jump (Some "JNE"));
  Alcotest.(check string) "same string" "110" (To_test.jump (Some "JLE"));
  Alcotest.(check string) "same string" "111" (To_test.jump (Some "JMP"))

let tests = [
  "dest returns code", `Quick, test_dest;
  "comp returns code", `Quick, test_comp;
  "jump returns code", `Quick, test_jump;
]
