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

let compile_keyword tokens outfile depth =
  let current = match tokens with
      [] -> raise ArgumentError
    | head :: _ -> head in
  match current with
    | "class" ->
      write_element_start "class" outfile depth;
      write_element "keyword" "class" outfile (depth + 1);
      write_element_end "class" outfile depth;
    | _ ->
      raise ArgumentError

(* 
let compile_identifier engine =
let compile_symbol engine = *)

(* 
let compile_class engine =
  write_element "class" engine
  compile_keyword *)

let _compile tokens outfile depth =
  match JackTokenizer.token_type tokens with
    | KEYWORD ->
      compile_keyword tokens outfile depth
    (* | SYMBOL ->
    | IDENTIFIER ->
    | INT_CONST ->
    | STRING_CONST -> *)
    | _ -> raise ArgumentError

let compile filepath =
  let tokens = JackTokenizer.create filepath in
  let outfile = (Batteries.String.rsplit filepath ~by: "." |> fst) ^ ".impl.xml" |> open_out in

  _compile tokens outfile 0;
  close_out outfile
  (* while JackTokenizer.has_more_tokens tokenizer do
    Printf.fprintf outfile "%s" (let a :: _ = tokenizer.tokens in a);
    JackTokenizer.advance tokenizer
  done; *)

