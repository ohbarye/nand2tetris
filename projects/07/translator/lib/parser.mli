type parser
type command = C_ARITHMETIC | C_PUSH | C_POP | C_LABEL | C_GOTO | C_IF | C_FUNCTION | C_RETURN | C_CALL
type arithmetic_command = ADD | SUB | AND | OR | EQ | GT | LT | NEG | NOT

val create : string -> parser
val has_more_commands : parser -> bool
val advance : parser -> unit
val command_type : parser -> command
val arithmetic_command_type : parser -> arithmetic_command
val arg1 : parser -> string
val arg2 : parser -> string
