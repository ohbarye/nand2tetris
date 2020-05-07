module SymbolTableMap = Map.Make(String);;

module SymbolTable : sig
  val create : unit -> int SymbolTableMap.t
  val add_entity : string -> 'a -> 'a SymbolTableMap.t -> 'a SymbolTableMap.t
  val contains : string -> 'a SymbolTableMap.t -> bool
  val get_address : string -> 'a option SymbolTableMap.t -> 'a option
end = struct
  let add_entity symbol address map =
    SymbolTableMap.add symbol address map

  let create () =
    SymbolTableMap.empty
      |> SymbolTableMap.add "SP" 0
      |> SymbolTableMap.add "LCL" 1
      |> SymbolTableMap.add "ARG" 2
      |> SymbolTableMap.add "THIS" 3
      |> SymbolTableMap.add "THAT" 4
      |> SymbolTableMap.add "R0" 0
      |> SymbolTableMap.add "R1" 1
      |> SymbolTableMap.add "R2" 2
      |> SymbolTableMap.add "R3" 3
      |> SymbolTableMap.add "R4" 4
      |> SymbolTableMap.add "R5" 5
      |> SymbolTableMap.add "R6" 6
      |> SymbolTableMap.add "R7" 7
      |> SymbolTableMap.add "R8" 8
      |> SymbolTableMap.add "R9" 9
      |> SymbolTableMap.add "R10" 10
      |> SymbolTableMap.add "R11" 11
      |> SymbolTableMap.add "R12" 12
      |> SymbolTableMap.add "R13" 13
      |> SymbolTableMap.add "R14" 14
      |> SymbolTableMap.add "R15" 15
      |> SymbolTableMap.add "SCREEN" 16384
      |> SymbolTableMap.add "KBD" 24576

  let contains symbol map =
    SymbolTableMap.mem symbol map

  let get_address symbol map =
    try
      SymbolTableMap.find symbol map
    with Not_found ->
      None
end
