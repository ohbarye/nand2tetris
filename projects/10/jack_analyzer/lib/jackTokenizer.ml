type tokenizer = {
  mutable file : in_channel;
  mutable has_next : bool;
  mutable current_token : string;
}

exception NoMoreTokens of string

let has_more_tokens tokenizer =
  tokenizer.has_next

let rec advance tokenizer =
  if not (has_more_tokens tokenizer) then raise (NoMoreTokens "No more commands")
  else
    try
      let s = Batteries.String.trim (input_line tokenizer.file) in
      if Batteries.String.is_empty s then
        advance tokenizer
      else
        tokenizer.current_token <- s
    with End_of_file ->
      tokenizer.has_next <- false;;

let create filepath =
  let infile = open_in filepath in
  let t = { file = infile; has_next = true; current_token = ""; } in
  advance t;
  t
