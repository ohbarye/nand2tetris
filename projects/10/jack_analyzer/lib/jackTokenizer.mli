type tokenType = KEYWORD | SYMBOL | IDENTIFIER | INT_CONST | STRING_CONST
type keywordType = CLASS | CONSTRUCTOR | FUNCTION | METHOD | FIELD | STATIC | VAR | INT | CHAR | BOOLEAN | VOID | TRUE | FALSE | NULL | THIS | LET | DO | IF | ELSE | WHILE | RETURN
type symbolType = LEFT_CURLY_BRACKET | RIGHT_CURLY_BRACKET | LEFT_ROUND_BRACKET | RIGHT_ROUND_BRACKET | LEFT_SQUARE_BRACKET | RIGHT_SQUARE_BRACKET | PERIOD | COMMA | SEMI_COLON | PLUS_SIGN | HYPHEN | ASTERISK | SLASH | AMPERSAND | VERTICAL_LINE | LESS_THAN_SIGN | GREATER_THAN_SIGN | EQUAL | TILDE

val create : string -> string list
val has_more_tokens : string list -> bool
val token_type : string list -> tokenType
val keyword : string list -> keywordType
val symbol : string list -> string
val identifier : string list -> string
val int_val : string list -> int
val string_val : string list -> string

(* For testing *)
val tokenize_line : string -> string list -> string list
val tokenize_unit : string -> string list -> string list
val tokenize : string -> string list

