type symbolTable
type identifierKind = FIELD | STATIC | VAR | ARG

val create : symbolTable
val define : string -> string -> identifierKind -> symbolTable -> symbolTable
val start_subroutine : symbolTable -> symbolTable
val kind_of : string -> symbolTable -> identifierKind option
val type_of : string -> symbolTable -> string option
val index_of : string -> symbolTable -> int option
