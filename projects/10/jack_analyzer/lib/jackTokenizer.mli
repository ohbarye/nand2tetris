type tokenizer

val create : string -> tokenizer
val has_more_tokens : tokenizer -> bool
val tokenize_line : string -> string list -> string list
val tokenize_unit : string -> string list -> string list
val tokenize : string -> string list

(* val advance : tokenizer -> unit *)

(*
type tokenType = KEYWORD | SYMBOL | IDENTIFIER | INT_CONST | STRING_CONST
type keywordType = CLASS | METHOD | FUNCTION | CONSTRUCTOR | INT | BOOLEAN | CHAR | VOID | VAR | STATIC | FIELD | LET | DO | IF | ELSE | WHILE | RETURN | TRUE | FALSE | NULL | THIS

val token_type : tokenizer -> tokenType
val keyword : tokenizer -> keywordType
val symbol : tokenizer -> char
val identifier : tokenizer -> string
val int_val : tokenizer -> int
val string_val : tokenizer -> string *)
