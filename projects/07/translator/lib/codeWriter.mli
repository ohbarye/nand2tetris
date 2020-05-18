open Parser

type writer

val create : string -> writer
val write_arithmetic : arithmetic_command -> writer -> unit
val write_push_pop : command -> string -> int -> writer -> unit
val close : writer -> unit
