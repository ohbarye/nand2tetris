open JackTokenizer

exception ArgumentError

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
  write_element "identifier" (List.hd tokens) outfile depth

let compile_symbol tokens outfile depth =
  write_element "symbol" (List.hd tokens) outfile depth

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
  write_element "keyword" current outfile (depth + 1);
  _compile rest outfile (depth + 1);
  let rest = List.tl rest in
  _compile rest outfile (depth + 1);
  write_element_end current outfile depth

and compile_keyword tokens outfile depth =
  match List.hd tokens with
    | "class" ->
      compile_class tokens outfile depth
    | _ ->
      raise ArgumentError

let compile filepath =
  let tokens = JackTokenizer.create filepath in
  let outfile = (Batteries.String.rsplit filepath ~by: "." |> fst) ^ ".impl.xml" |> open_out in
  _compile tokens outfile 0;
  close_out outfile
