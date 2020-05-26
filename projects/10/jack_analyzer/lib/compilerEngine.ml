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
  let (current, rest) = match tokens with
      [] -> raise ArgumentError
    | head :: tail -> (head, tail) in
  write_element_start current outfile depth;
  write_element "keyword" current outfile (depth + 1); (* 'class' *)
  let rest = _compile rest outfile (depth + 1) in (* className *)
  let rest = _compile rest outfile (depth + 1) in (* '{' *)

  let rest = ref rest in
  while (List.hd !rest) <> "}" do
    rest := ["}"]
  done;
  let rest = !rest in

  let rest = _compile rest outfile (depth + 1) in (* '}' *)
  write_element_end current outfile depth;
  rest

and compile_keyword tokens outfile depth =
  match List.hd tokens with
    | "class" ->
      compile_class tokens outfile depth
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
