open Parser

type writer

val create : string -> writer
val set_file_name : string -> writer -> unit
val write_arithmetic : arithmetic_command -> writer -> unit
val write_push_pop : command -> string -> int -> writer -> unit
val write_label : string -> writer -> unit
val write_goto : string -> writer -> unit
val write_if : string -> writer -> unit
val write_function : string -> int -> writer -> unit
val write_return : writer -> unit
val write_call : string -> int -> writer -> unit
val close : writer -> unit
