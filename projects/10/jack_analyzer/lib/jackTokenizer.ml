type tokenizer = {
  mutable file : string;
  mutable has_next : bool;
  mutable current_token : string;
}

(* exception NoMoreTokens of string *)

let has_more_tokens tokenizer =
  tokenizer.has_next

(* let rec advance tokenizer =
  if not (has_more_tokens tokenizer) then raise (NoMoreTokens "No more commands")
  else
    try
      let s = Batteries.String.trim (input_line tokenizer.file) in
      if Batteries.String.is_empty s then
        advance tokenizer
      else
        tokenizer.current_token <- s
    with End_of_file ->
      tokenizer.has_next <- false;; *)

let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let rec delete_singleline_comment content =
  if Batteries.String.exists content "//" then
    let index_from = Batteries.String.rfind content "//" in
    Batteries.String.slice ~first: (index_from + 2) content
      |> delete_singleline_comment
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

let delete_comment content =
  delete_multiline_comment content
    |> delete_singleline_comment

let split_without_empty_lines content =
  Batteries.String.split_on_string content ~by: "\n"
    |> List.map Batteries.String.trim
    |> List.filter (fun x -> not (Batteries.String.is_empty x))

let create filepath =
  let lines = read_whole_file filepath
    |> delete_comment
    |> split_without_empty_lines in

  List.iter (fun x -> print_endline x) lines;
  
  let t = { file = ""; has_next = false; current_token = ""; } in
  (* advance t; *)
  t
