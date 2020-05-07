module SymbolTableMap = Map.Make(String);;

module SymbolTable : sig
  val empty : unit -> 'a SymbolTableMap.t
  val add_entity : string -> 'a -> 'a SymbolTableMap.t -> 'a SymbolTableMap.t
  val contains :  string -> 'a SymbolTableMap.t -> bool
  val get_address : string -> 'a option SymbolTableMap.t -> 'a option
end = struct
  let empty () =
    SymbolTableMap.empty

  let add_entity symbol address map =
    SymbolTableMap.add symbol address map

  let contains symbol map =
    SymbolTableMap.mem symbol map

  let get_address symbol map =
    try
      SymbolTableMap.find symbol map
    with Not_found ->
      None
end
