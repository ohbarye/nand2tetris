open JackTokenizer

exception ArgumentError
exception CompileError of string

let write_element tag_name value outfile depth =
  let indention = Batteries.String.repeat "  " depth in
  Printf.fprintf outfile "%s" (indention ^ "<" ^ tag_name ^ "> " ^ value ^ " </" ^ tag_name ^ ">\n")

let write_element_start tag_name outfile depth =
  let indention = Batteries.String.repeat "  " depth in
  Printf.fprintf outfile "%s" (indention ^ "<" ^ tag_name ^ ">\n")

let write_element_end tag_name outfile depth =
  let indention = Batteries.String.repeat "  " depth in
  Printf.fprintf outfile "%s" (indention ^ "</" ^ tag_name ^ ">\n")

let compile_identifier outfile depth tokens =
  write_element "identifier" (List.hd tokens) outfile depth;
  List.tl tokens

let compile_symbol outfile depth tokens =
  let symbol = match List.hd tokens with
    | "<" -> "&lt;"
    | ">" -> "&gt;"
    | "&" -> "&amp;"
    | s -> s in
  write_element "symbol" symbol outfile depth;
  List.tl tokens

let compile_integer_const outfile depth tokens =
  write_element "integerConstant" (List.hd tokens) outfile depth;
  List.tl tokens

let sanitize_string str =
  if Batteries.String.starts_with str "\"" then
    Batteries.String.slice ~first:1 ~last:(-1) str
  else
    str

let compile_string_const outfile depth tokens =
  let sanitized = sanitize_string (List.hd tokens) in
  write_element "stringConstant" sanitized outfile depth;
  List.tl tokens

let rec _compile outfile depth tokens =
  match JackTokenizer.token_type tokens with
    | KEYWORD ->
      compile_keyword outfile depth tokens
    | IDENTIFIER ->
      compile_identifier outfile depth tokens
    | SYMBOL ->
      compile_symbol outfile depth tokens
    | INT_CONST ->
      compile_integer_const outfile depth tokens
    | STRING_CONST ->
      compile_string_const outfile depth tokens

and compile_class_var_dec_var_name outfile depth tokens =
  match List.hd tokens with
    | ";" -> tokens
    | "," ->
      _compile outfile depth tokens (* ',' *)
        |> _compile outfile depth (* varName *)
    | s ->
      raise (CompileError (Printf.sprintf "Unknown token is given in class_var_dec: %s" s))

and compile_class_var_dec outfile depth tokens =
  let (current, rest) = match tokens with
      [] -> raise ArgumentError
    | head :: tail -> (head, tail) in
  write_element_start "classVarDec" outfile depth;
  write_element "keyword" current outfile (depth + 1); (* ('static'|'field') *)
  let rest = _compile outfile (depth + 1) rest (* type *)
    |> _compile outfile (depth + 1) (* varName *)
    |> compile_class_var_dec_var_name outfile (depth + 1)
    |> _compile outfile (depth + 1) in (* ';' *)
  write_element_end "classVarDec" outfile depth;
  rest

and compile_class_var_dec_or_subroutine_dec outfile depth tokens =
  match List.hd tokens with
    | "}" -> tokens
    | "static" | "field" ->
      compile_class_var_dec outfile depth tokens
        |> compile_class_var_dec_or_subroutine_dec outfile depth
    | "constructor" | "function" | "method" ->
      compile_subroutine_dec outfile depth tokens
        |> compile_class_var_dec_or_subroutine_dec outfile depth
    | s ->
      raise (CompileError (Printf.sprintf "Unknown token is given under class: %s" s))

and compile_class outfile depth tokens =
  write_element_start "class" outfile depth;
  write_element "keyword" "class" outfile (depth + 1); (* 'class' *)

  let rest = List.tl tokens
    |> _compile outfile (depth + 1) (* className *)
    |> _compile outfile (depth + 1) (* '{' *)
    |> compile_class_var_dec_or_subroutine_dec outfile (depth + 1)
    |> _compile outfile (depth + 1) in (* '}' *)
  write_element_end "class" outfile depth;
  rest

and _compile_parameter_list outfile depth tokens =
  match tokens with
    | _ :: _ :: ")" :: _ ->
      _compile outfile depth tokens (* type *)
        |> _compile outfile depth (* varName *)
        |> _compile_parameter_list outfile depth
    | _ :: _ :: "," :: _ ->
      _compile outfile depth tokens (* ',' *)
        |> _compile outfile depth (* type *)
        |> _compile outfile depth (* varName *)
        |> _compile_parameter_list outfile depth
    | _ ->
      tokens

and compile_parameter_list outfile depth tokens =
  write_element_start "parameterList" outfile depth;
  let rest = _compile_parameter_list outfile (depth + 1) tokens in
  write_element_end "parameterList" outfile depth;
  rest

and compile_subroutine_body_var_dec outfile depth tokens =
  match List.hd tokens with
    | "var" ->
      _compile outfile depth tokens
        |> compile_subroutine_body_var_dec outfile depth
    | _ -> tokens

and _compile_expression_list outfile depth tokens =
  match tokens with
    | "," :: _ ->
        _compile outfile depth tokens (* ',' *)
        |> compile_expression outfile depth
        |> _compile_expression_list outfile depth
    | ")" :: _ ->
      tokens
    | _ ->
      compile_expression outfile depth tokens
        |> _compile_expression_list outfile depth

and compile_expression_list outfile depth tokens =
  write_element_start "expressionList" outfile depth;
  let rest = _compile_expression_list outfile (depth + 1) tokens in
  write_element_end "expressionList" outfile depth;
  rest

and compile_subroutine_call outfile depth tokens =
  match tokens with
    | _ :: "(" :: _ ->
      _compile outfile depth tokens (* subroutineName *)
        |> _compile outfile depth (* '(' *)
        |> compile_expression_list outfile depth (* expressionList *)
        |> _compile outfile depth (* ')' *)
    | _ :: "." :: _ ->
      _compile outfile depth tokens (* (className|varName) *)
        |> _compile outfile depth (* '.' *)
        |> _compile outfile depth (* subroutineName *)
        |> _compile outfile depth (* '(' *)
        |> compile_expression_list outfile depth (* expressionList *)
        |> _compile outfile depth (* ')' *)
    | _ ->
      raise (CompileError "Syntax error: subroutine call")

and _compile_term outfile depth tokens =
  match JackTokenizer.token_type tokens with
    | INT_CONST | STRING_CONST ->
      _compile outfile depth tokens
    | KEYWORD ->
        (match List.hd tokens with
          | "null" | "this" | "true" | "false" ->
            _compile outfile depth tokens
          | s -> raise (CompileError (Printf.sprintf "Unknown token is given: %s" s))
       )
    | SYMBOL ->
      (match List.hd tokens with
        | "(" ->
          _compile outfile depth tokens (* '(' *)
            |> compile_expression outfile depth
            |> _compile outfile depth (* ')' *)
        | "-" | "~" ->
          _compile outfile depth tokens (* unaryOp *)
            |> compile_term outfile depth
        | s -> raise (CompileError (Printf.sprintf "Unknown token is given: %s" s))
      )
    | IDENTIFIER ->
      (match tokens with
        | _ :: "[" :: _ ->
          _compile outfile depth tokens (* varName *)
            |> _compile outfile depth (* '[' *)
            |> compile_expression outfile depth (* expression *)
            |> _compile outfile depth (* ']' *)
        | _ :: "(" :: _ | _ :: "." :: _ ->
          compile_subroutine_call outfile depth tokens (* subroutineCall *)
        | _ ->
          _compile outfile depth tokens (* varName *)
      )
 
and compile_term outfile depth tokens =
  write_element_start "term" outfile depth;
  let rest = _compile_term outfile (depth + 1) tokens in
  write_element_end "term" outfile depth;
  rest

and _compile_expression outfile depth tokens =
  match List.hd tokens with
    | "+" | "-" | "*" | "/" | "&" | "|" | "<" | ">" | "=" ->
      _compile outfile depth tokens
        |> compile_term outfile depth
        |> _compile_expression outfile depth
    | _ -> tokens

and compile_expression outfile depth tokens =
  write_element_start "expression" outfile depth;
  let rest = compile_term outfile (depth + 1) tokens
    |> _compile_expression outfile (depth + 1) in
  write_element_end "expression" outfile depth;
  rest

and compile_let_statement_index outfile depth tokens =
  match List.hd tokens with
    | "=" -> tokens
    | "[" ->
      _compile outfile depth tokens (* '[' *)
        |> compile_expression outfile depth (* expression *)
        |> _compile outfile depth (* ']' *)
    | s ->
      raise (CompileError (Printf.sprintf "Unexpected token is given in let statement: %s" s))

and compile_let_statement outfile depth tokens =
  write_element_start "letStatement" outfile depth;
  write_element "keyword" "let" outfile (depth + 1); (* 'let' *)
  let rest = _compile outfile (depth + 1) (List.tl tokens) (* varName *)
    |> compile_let_statement_index outfile (depth + 1)
    |> _compile outfile (depth + 1) (* '=' *)
    |> compile_expression outfile (depth + 1) (* expression *)
    |> _compile outfile (depth + 1) in (* ';' *)
  write_element_end "letStatement" outfile depth;
  rest

and compile_while_statement outfile depth tokens =
  write_element_start "whileStatement" outfile depth;
  write_element "keyword" "while" outfile (depth + 1); (* 'while' *)
  let rest = _compile outfile (depth + 1) (List.tl tokens) (* '(' *)
    |> compile_expression outfile (depth + 1) (* expression *)
    |> _compile outfile (depth + 1) (* ')' *)
    |> _compile outfile (depth + 1) (* '{' *)
    |> compile_statements outfile (depth + 1) (* statements *)
    |> _compile outfile (depth + 1) in (* '}' *)
  write_element_end "whileStatement" outfile depth;
  rest

and _compile_return_statement outfile depth tokens =
  match List.hd tokens with
    | ";" -> tokens
    | _ ->
      compile_expression outfile depth tokens

and compile_return_statement outfile depth tokens =
  write_element_start "returnStatement" outfile depth;
  write_element "keyword" "return" outfile (depth + 1); (* 'return' *)
  let rest = _compile_return_statement outfile (depth + 1) (List.tl tokens)
    |> _compile outfile (depth + 1) in (* ';' *)
  write_element_end "returnStatement" outfile depth;
  rest

and compile_do_statement outfile depth tokens =
  write_element_start "doStatement" outfile depth;
  write_element "keyword" "do" outfile (depth + 1); (* 'do' *)
  let rest = compile_subroutine_call outfile (depth + 1) (List.tl tokens) (* subroutineCall *)
    |> _compile outfile (depth + 1) in (* ';' *)
  write_element_end "doStatement" outfile depth;
  rest

and compile_if_statement_else outfile depth tokens =
  match List.hd tokens with
    | "else" ->
      write_element "keyword" "else" outfile depth; (* 'else' *)
      _compile outfile depth (List.tl tokens) (* '{' *)
      |> compile_statements outfile depth (* statements *)
      |> _compile outfile depth (* '}' *)
    | _ ->
      tokens

and compile_if_statement outfile depth tokens =
  write_element_start "ifStatement" outfile depth;
  write_element "keyword" "if" outfile (depth + 1); (* 'if' *)
  let rest = _compile outfile (depth + 1) (List.tl tokens) (* '(' *)
    |> compile_expression outfile (depth + 1) (* expression *)
    |> _compile outfile (depth + 1) (* ')' *)
    |> _compile outfile (depth + 1) (* '{' *)
    |> compile_statements outfile (depth + 1) (* statements *)
    |> _compile outfile (depth + 1) (* '}' *)
    |> compile_if_statement_else outfile (depth + 1) in
  write_element_end "ifStatement" outfile depth;
  rest

and compile_statements_repeat outfile depth tokens =
  match List.hd tokens with
    | "let" | "if" | "while" | "do" | "return" ->
      _compile outfile depth tokens
        |> compile_statements_repeat outfile depth
    | _ -> tokens

and compile_statements outfile depth tokens =
  write_element_start "statements" outfile depth;
  let rest = compile_statements_repeat outfile (depth + 1) tokens in
  write_element_end "statements" outfile depth;
  rest

and compile_subroutine_body outfile depth tokens =
  write_element_start "subroutineBody" outfile depth;
  let rest = _compile outfile (depth + 1) tokens (* '{' *)
    |> compile_subroutine_body_var_dec outfile (depth + 1) (* varDec *)
    |> compile_statements outfile (depth + 1) (* statements* *)
    |> _compile outfile (depth + 1) in (* '}' *)
  write_element_end "subroutineBody" outfile depth;
  rest

and compile_subroutine_dec outfile depth tokens =
  let (current, rest) = match tokens with
      [] -> raise ArgumentError
    | head :: tail -> (head, tail) in
  write_element_start "subroutineDec" outfile depth;
  write_element "keyword" current outfile (depth + 1); (* ('constructor'|'function'|'method') *)
  let rest = _compile outfile (depth + 1) rest (* ('void'|type) *)
    |> _compile outfile (depth + 1) (* subroutineName *)
    |> _compile outfile (depth + 1) (* '(' *)
    |> compile_parameter_list outfile (depth + 1) (* parameterList *)
    |> _compile outfile (depth + 1) (* ')' *)
    |> compile_subroutine_body outfile (depth + 1) in (* subroutineBody *)
  write_element_end "subroutineDec" outfile depth;
  rest

and compile_var_dec_repeat outfile depth tokens =
  match List.hd tokens with
    | ";" -> tokens
    | _ ->
      _compile outfile depth tokens (* ',' *)
      |>  _compile outfile depth (* varName *)

and compile_var_dec outfile depth tokens =
  write_element_start "varDec" outfile depth;
  write_element "keyword" "var" outfile (depth + 1); (* 'var' *)
  let rest = List.tl tokens
    |> _compile outfile (depth + 1) (* type *)
    |> _compile outfile (depth + 1) (* varName *)
    |> compile_var_dec_repeat outfile (depth + 1)
    |> _compile outfile (depth + 1) in (* ';' *)
  write_element_end "varDec" outfile depth;
  rest

and compile_keyword_constant outfile depth tokens =
  write_element "keyword" (List.hd tokens) outfile depth;
  List.tl tokens

and compile_type outfile depth tokens =
  write_element "keyword" (List.hd tokens) outfile depth;
  List.tl tokens

and compile_keyword outfile depth tokens =
  match List.hd tokens with
    | "class" ->
      compile_class outfile depth tokens
    | "constructor" | "function" | "method" ->
      compile_subroutine_dec outfile depth tokens
    | "true" | "false" | "null" | "this" ->
      compile_keyword_constant outfile depth tokens
    | "void" | "int" | "char" | "boolean" ->
      compile_type outfile depth tokens
    | "var" ->
      compile_var_dec outfile depth tokens
    | "let" ->
      compile_let_statement outfile depth tokens
    | "while" ->
      compile_while_statement outfile depth tokens
    | "do" ->
      compile_do_statement outfile depth tokens
    | "return" ->
      compile_return_statement outfile depth tokens
    | "if" ->
      compile_if_statement outfile depth tokens
    | _ ->
      raise (CompileError (Printf.sprintf "This token is unrecognized: %s" (List.hd tokens)))

let check_rest = function
  | [] -> ()
  | rest ->
    raise (CompileError (Printf.sprintf "All tokens are not consumed. rest: %s" (Batteries.String.join "" rest)))

let compile filepath =
  let tokens = JackTokenizer.create filepath in
  let outfile = (Batteries.String.rsplit filepath ~by: "." |> fst) ^ ".impl.xml" |> open_out in
  _compile outfile 0 tokens
    |> check_rest;
  close_out outfile
