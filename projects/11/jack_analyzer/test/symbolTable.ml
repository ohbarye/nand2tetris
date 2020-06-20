module To_test = struct
  (* let table = Nand2tetris_compiler.SymbolTable.Table *)
  let create = Nand2tetris_compiler.SymbolTable.create
  let define = Nand2tetris_compiler.SymbolTable.define
end

module Table = Map.Make(String)

let test_create () =
  let table = To_test.create in
  Alcotest.(check bool "same list" (Table.mem "" table.static_table) false)

let tests = [
  "test_tokenize_unit", `Quick, test_create;
]