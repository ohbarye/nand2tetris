type parser
type command = A_COMMAND | C_COMMAND | L_COMMAND

val create : in_channel -> parser
val has_more_commands : parser -> bool
val advance : parser -> unit
val command_type : parser -> command
val comp : parser -> string
val dest : parser -> string option
val jump : parser -> string option
val symbol : parser -> string
