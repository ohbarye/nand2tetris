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
  write_element "symbol" (List.hd tokens) outfile depth;
  List.tl tokens

let rec _compile outfile depth tokens =
  match JackTokenizer.token_type tokens with
    | KEYWORD ->
      compile_keyword outfile depth tokens
    | IDENTIFIER ->
      compile_identifier outfile depth tokens
    | SYMBOL ->
      compile_symbol outfile depth tokens
(*    INT_CONST ->
    | STRING_CONST -> *)
    | _ -> raise ArgumentError
and compile_class_var_dec_or_subroutine_dec outfile depth tokens =
  match List.hd tokens with
    | "}" -> tokens
    | _ ->
      _compile outfile depth tokens
        |> compile_class_var_dec_or_subroutine_dec outfile depth

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

and compile_parameter_list outfile depth tokens =
  write_element_start "parameterList" outfile depth;
  (* TODO *)
  write_element_end "parameterList" outfile depth;
  tokens

and compile_subroutine_body_var_dec outfile depth tokens =
  match List.hd tokens with
    | "var" ->
      _compile outfile depth tokens
        |> compile_subroutine_body_var_dec outfile depth
    | _ -> tokens

and compile_subroutine_body outfile depth tokens =
  write_element_start "subroutineBody" outfile depth;
  let rest = _compile outfile (depth + 1) tokens (* '{' *)
    |> compile_subroutine_body_var_dec outfile (depth + 1) (* varDec *)
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

and compile_type outfile depth tokens =
  write_element "keyword" (List.hd tokens) outfile depth;
  List.tl tokens

and compile_keyword outfile depth tokens =
  match List.hd tokens with
    | "class" ->
      compile_class outfile depth tokens
    | "constructor" | "function" | "method" ->
      compile_subroutine_dec outfile depth tokens
    | "void" | "int" | "char" | "boolean" ->
      compile_type outfile depth tokens
    | "var" ->
      compile_var_dec outfile depth tokens
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
    |> check_rest
