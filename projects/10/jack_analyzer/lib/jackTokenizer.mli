type tokenizer
type tokenType
type keywordType
type symbolType

val create : string -> tokenizer
val has_more_tokens : tokenizer -> bool
val advance : tokenizer -> unit
val token_type : tokenizer -> tokenType
val keyword : tokenizer -> keywordType
val symbol : tokenizer -> string
val identifier : tokenizer -> string
val int_val : tokenizer -> int
val string_val : tokenizer -> string

(* For debug *)
val tokenize_line : string -> string list -> string list
val tokenize_unit : string -> string list -> string list
val tokenize : string -> string list

