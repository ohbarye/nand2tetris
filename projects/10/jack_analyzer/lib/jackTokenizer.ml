type tokenType = KEYWORD | SYMBOL | IDENTIFIER | INT_CONST | STRING_CONST
type keywordType = CLASS | CONSTRUCTOR | FUNCTION | METHOD | FIELD | STATIC | VAR | INT | CHAR | BOOLEAN | VOID | TRUE | FALSE | NULL | THIS | LET | DO | IF | ELSE | WHILE | RETURN
type symbolType = LEFT_CURLY_BRACKET | RIGHT_CURLY_BRACKET | LEFT_ROUND_BRACKET | RIGHT_ROUND_BRACKET | LEFT_SQUARE_BRACKET | RIGHT_SQUARE_BRACKET | PERIOD | COMMA | SEMI_COLON | PLUS_SIGN | HYPHEN | ASTERISK | SLASH | AMPERSAND | VERTICAL_LINE | LESS_THAN_SIGN | GREATER_THAN_SIGN | EQUAL | TILDE

exception ArgumentError

module KeywordMap = Map.Make(String)
let keyword_map =
  KeywordMap.empty
    |> KeywordMap.add "class" CLASS
    |> KeywordMap.add "constructor" CONSTRUCTOR
    |> KeywordMap.add "function" FUNCTION
    |> KeywordMap.add "method" METHOD
    |> KeywordMap.add "field" FIELD
    |> KeywordMap.add "static" STATIC
    |> KeywordMap.add "var" VAR
    |> KeywordMap.add "int" INT
    |> KeywordMap.add "char" CHAR
    |> KeywordMap.add "boolean" BOOLEAN
    |> KeywordMap.add "void" VOID
    |> KeywordMap.add "true" TRUE
    |> KeywordMap.add "false" FALSE
    |> KeywordMap.add "null" NULL
    |> KeywordMap.add "this" THIS
    |> KeywordMap.add "let" LET
    |> KeywordMap.add "do" DO
    |> KeywordMap.add "if" IF
    |> KeywordMap.add "else" ELSE
    |> KeywordMap.add "while" WHILE
    |> KeywordMap.add "return" RETURN

module SymbolMap = Map.Make(String)
let symbol_map =
  SymbolMap.empty
    |> SymbolMap.add "{" LEFT_CURLY_BRACKET
    |> SymbolMap.add "}" RIGHT_CURLY_BRACKET
    |> SymbolMap.add "(" LEFT_ROUND_BRACKET
    |> SymbolMap.add ")" RIGHT_ROUND_BRACKET
    |> SymbolMap.add "[" LEFT_SQUARE_BRACKET
    |> SymbolMap.add "]" RIGHT_SQUARE_BRACKET
    |> SymbolMap.add "." PERIOD
    |> SymbolMap.add "," COMMA
    |> SymbolMap.add ";" SEMI_COLON
    |> SymbolMap.add "+" PLUS_SIGN
    |> SymbolMap.add "-" HYPHEN
    |> SymbolMap.add "*" ASTERISK
    |> SymbolMap.add "/" SLASH
    |> SymbolMap.add "&" AMPERSAND
    |> SymbolMap.add "|" VERTICAL_LINE
    |> SymbolMap.add "<" LESS_THAN_SIGN
    |> SymbolMap.add ">" GREATER_THAN_SIGN
    |> SymbolMap.add "=" EQUAL
    |> SymbolMap.add "~" TILDE

let has_more_tokens tokens =
  match tokens with
      [] -> false
    | _ -> true

let reg = Str.regexp "[-\\{\\}\\(\\)\\[\\.,;\\+\\*\\/&\\|<>=~]\\|\\]"

let max_int = 32767
let int_regex = Str.regexp "^[0-9]+$"
let identifier_regex = Str.regexp "^[a-zA-Z_][a-zA-Z0-9_]*$"
let string_regex = Str.regexp "^\"[^\"\n]*\"$"

let current_token tokens =
  match tokens with
      [] -> raise ArgumentError
    | head :: _ -> head

let token_type tokens =
  match current_token tokens with
    | s when KeywordMap.mem s keyword_map
      -> KEYWORD
    | s when SymbolMap.mem s symbol_map
      -> SYMBOL
    | s when (Str.string_match int_regex s 0) && (int_of_string s) <= max_int
      -> INT_CONST
    | s when Str.string_match string_regex s 0
      -> STRING_CONST
    | s when Str.string_match identifier_regex s 0
      -> IDENTIFIER
    | _
      -> raise ArgumentError

let keyword tokens =
  KeywordMap.find (current_token tokens) keyword_map

let symbol tokens =
  current_token tokens

let identifier tokens =
  current_token tokens

let int_val tokens =
  int_of_string (current_token tokens)

let string_val tokens =
  current_token tokens

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let delete_singleline_comment line =
  if Batteries.String.exists line "//" then
    let index_from = Batteries.String.find line "//" in
    Batteries.String.slice ~first: 0 ~last: index_from line
  else
    line

let rec delete_multiline_comment content =
  if Batteries.String.exists content "/*" then
    let index_from = Batteries.String.find content "/*" in
    let index_to   = Batteries.String.find content "*/" in
    (Batteries.String.slice ~first: 0 ~last: index_from content) ^ (Batteries.String.slice ~first: (index_to + 2) content)
      |> delete_multiline_comment
  else
    content

let split_without_empty_lines content =
  Batteries.String.split_on_string content ~by: "\n"
    |> List.map delete_singleline_comment
    |> List.map Batteries.String.trim
    |> List.filter (fun x -> not (Batteries.String.is_empty x))

let rec tokenize_unit unit tokens =
  match unit with
      "" -> tokens
    | unit ->
      try
        let index = Str.search_forward reg unit 0 in
        let token = if index <> 0 then
            [Batteries.String.slice ~first: 0 ~last: index unit] else [] in
        let rest_unit = Batteries.String.slice ~first: (index + 1) unit in
        let token = token @ [Batteries.String.slice ~first: index ~last: (index + 1) unit] in
        tokenize_unit rest_unit (tokens @ token)
      with Not_found ->
        tokenize_unit "" (tokens @ [unit])

let before_double_quote line =
  Batteries.String.exists line "\"" && Batteries.String.exists line " " &&
    (
      let dq_index = Batteries.String.find line "\"" in
      let space_index = Batteries.String.find line " " in
      dq_index < space_index
    )

let rec tokenize_line line tokens =
  match line with
      "" -> tokens
    | line when Batteries.String.starts_with line "\"" ->
      let index = Batteries.String.find_from line 1 "\"" in
      let rest_line = Batteries.String.slice ~first: (index + 1) line in
      let token = Batteries.String.slice ~first: 0 ~last: (index + 1) line in
      tokenize_line rest_line (tokens @ [token])
    | line when before_double_quote line ->
      let index = Batteries.String.find line "\"" in
      let unit = Batteries.String.slice ~first: 0 ~last: index line in
      let rest_line = Batteries.String.slice ~first: index line
        |> Batteries.String.trim in
      tokenize_line rest_line (tokenize_unit unit tokens)
    | line when Batteries.String.exists line " " ->
      let index = Batteries.String.find line " " in
      let unit = Batteries.String.slice ~first: 0 ~last: index line in
      let rest_line = Batteries.String.slice ~first: (index + 1) line
        |> Batteries.String.trim in
      tokenize_line rest_line (tokenize_unit unit tokens)
    | unit ->
      tokenize_line "" (tokenize_unit unit tokens)

let rec tokenize_all_lines lines tokens =
  match lines with 
    [] -> tokens
  | line :: rest ->
    tokenize_all_lines rest (tokenize_line line tokens)

let tokenize content =
  let lines = delete_multiline_comment content
    |> split_without_empty_lines in
  tokenize_all_lines lines []

let create filepath =
  read_whole_file filepath
    |> tokenize
