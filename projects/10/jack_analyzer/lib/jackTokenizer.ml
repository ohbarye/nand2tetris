type tokenizer = {
  mutable file : string;
  mutable has_next : bool;
  mutable current_token : string;
}

let has_more_tokens tokenizer =
  tokenizer.has_next

let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let delete_singleline_comment content =
  if Batteries.String.exists content "//" then
    let index_from = Batteries.String.find content "//" in
    Batteries.String.slice ~first: 0 ~last: index_from content
  else
    content

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

let reg = Str.regexp "[-\\{\\}\\(\\)\\[\\.,;\\+\\*\\/&\\|<>=~]\\|\\]"

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
  if Batteries.String.exists line "\"" && Batteries.String.exists line " " then
      let dq_index = Batteries.String.find line "\"" in
      let space_index = Batteries.String.find line " " in
      dq_index < space_index
  else
    false

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
  let tokens = read_whole_file filepath
    |> tokenize in

  List.iter (fun _ -> ()) tokens;
  List.iter (fun x -> print_endline x) tokens;
  
  let t = { file = ""; has_next = false; current_token = ""; } in
  (* advance t; *)
  t
