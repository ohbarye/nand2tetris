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

let compile_identifier tokens outfile depth =
  write_element "identifier" (List.hd tokens) outfile depth;
  List.tl tokens

let compile_symbol tokens outfile depth =
  write_element "symbol" (List.hd tokens) outfile depth;
  List.tl tokens

let rec _compile tokens outfile depth =
  match JackTokenizer.token_type tokens with
    | KEYWORD ->
      compile_keyword tokens outfile depth
    | IDENTIFIER ->
      compile_identifier tokens outfile depth
    | SYMBOL ->
      compile_symbol tokens outfile depth
(*    INT_CONST ->
    | STRING_CONST -> *)
    | _ -> raise ArgumentError

and compile_class tokens outfile depth =
  write_element_start "class" outfile depth;
  write_element "keyword" "class" outfile (depth + 1); (* 'class' *)

  let rest = List.tl tokens in
  let rest = _compile rest outfile (depth + 1) in (* className *)
  let rest = _compile rest outfile (depth + 1) in (* '{' *)

  let rest = ref rest in
  while (List.hd !rest) <> "}" do
    rest :=  _compile !rest outfile (depth + 1) (* classVarDec or subroutineDec *)
  done;
  let rest = !rest in

  let rest = _compile rest outfile (depth + 1) in (* '}' *)
  write_element_end "class" outfile depth;
  rest

and compile_parameter_list tokens outfile depth =
  write_element_start "parameterList" outfile depth;
  (* TODO *)
  write_element_end "parameterList" outfile depth;
  tokens

and compile_subroutine_body tokens outfile depth =
  write_element_start "subroutineBody" outfile depth;
  let rest = _compile tokens outfile (depth + 1) in (* '{' *)

  let rest = ref rest in
  while (List.hd !rest) = "var" do
    rest :=  _compile !rest outfile (depth + 1) (* varDec *)
  done;
  let rest = !rest in

  let rest = _compile rest outfile (depth + 1) in (* '}' *)
  write_element_end "subroutineBody" outfile depth;
  rest

and compile_subroutine_dec tokens outfile depth =
  let (current, rest) = match tokens with
      [] -> raise ArgumentError
    | head :: tail -> (head, tail) in
  write_element_start "subroutineDec" outfile depth;
  write_element "keyword" current outfile (depth + 1); (* ('constructor'|'function'|'method') *)
  let rest = _compile rest outfile (depth + 1) in (* ('void'|type) *)
  let rest = _compile rest outfile (depth + 1) in (* subroutineName *)
  let rest = _compile rest outfile (depth + 1) in (* '(' *)
  let rest = compile_parameter_list rest outfile (depth + 1) in (* parameterList *)
  let rest = _compile rest outfile (depth + 1) in (* ')' *)
  let rest = compile_subroutine_body rest outfile (depth + 1) in (* subroutineBody *)
  write_element_end "subroutineDec" outfile depth;
  rest

and compile_var_dec tokens outfile depth =
  let (_, rest) = match tokens with
      [] -> raise ArgumentError
    | head :: tail -> (head, tail) in
  write_element_start "varDec" outfile depth;
  write_element "keyword" "var" outfile (depth + 1); (* 'var' *)
  let rest = _compile rest outfile (depth + 1) in (* type *)
  let rest = _compile rest outfile (depth + 1) in (* varName *)

  let rest = ref rest in
  while (List.hd !rest) <> ";" do
    rest :=  _compile !rest outfile (depth + 1); (* ',' *)
    rest :=  _compile !rest outfile (depth + 1) (* varName *)
  done;
  let rest = !rest in

  let rest = _compile rest outfile (depth + 1) in (* ';' *)
  write_element_end "varDec" outfile depth;
  rest

and compile_type tokens outfile depth =
  write_element "keyword" (List.hd tokens) outfile depth;
  List.tl tokens

and compile_keyword tokens outfile depth =
  match List.hd tokens with
    | "class" ->
      compile_class tokens outfile depth
    | "constructor" | "function" | "method" ->
      compile_subroutine_dec tokens outfile depth
    | "void" | "int" | "char" | "boolean" ->
      compile_type tokens outfile depth
    | "var" ->
      compile_var_dec tokens outfile depth
    | _ ->
      raise (CompileError (Printf.sprintf "This token is unrecognized: %s" (List.hd tokens)))

let check_rest = function
  | [] -> ()
  | rest ->
    raise (CompileError (Printf.sprintf "All tokens are not consumed. rest: %s" (Batteries.String.join "" rest)))

let compile filepath =
  let tokens = JackTokenizer.create filepath in
  let outfile = (Batteries.String.rsplit filepath ~by: "." |> fst) ^ ".impl.xml" |> open_out in
  let rest = _compile tokens outfile 0 in
  check_rest rest
