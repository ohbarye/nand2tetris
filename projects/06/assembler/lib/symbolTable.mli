module SymbolTableMap : Map.S with type key = string

val create : unit -> int SymbolTableMap.t
val add_entity : string -> 'a -> 'a SymbolTableMap.t -> 'a SymbolTableMap.t
val contains : string -> 'a SymbolTableMap.t -> bool
val get_address : string -> 'a SymbolTableMap.t -> 'a
