type parser = {
   mutable file : in_channel;
   mutable has_next : bool;
   mutable current_line : string;
}

type command = A_COMMAND | C_COMMAND | L_COMMAND

exception NoMoreCommands of string
exception UnhandledOperation of string

let has_more_commands p = p.has_next

let command_type p =
  match String.get p.current_line 0 with
    | '@' -> A_COMMAND
    | '(' -> L_COMMAND
    | _ -> C_COMMAND

let trim str =
  if Batteries.String.exists str "//" then
    Batteries.String.split str ~by: "//"
      |> fst
      |> Batteries.String.trim
  else
    Batteries.String.trim str

let rec advance p =
  if not (has_more_commands p) then raise (NoMoreCommands "No more commands")
  else
    try
      let s = trim (input_line p.file) in
      if Batteries.String.is_empty s then
        advance p
      else
        p.current_line <- s
    with End_of_file ->
      p.has_next <- false;;

let create input =
  let p = { file = input; has_next = true; current_line = "" } in
  advance p;
  p

let dest p =
  if (command_type p) <> C_COMMAND then raise (UnhandledOperation "dest is only for C instruction");
  if Batteries.String.exists p.current_line "=" then
    Some (Batteries.String.split p.current_line ~by: "=" |> fst)
  else
    None

let comp p =
  if (command_type p) <> C_COMMAND then raise (UnhandledOperation "comp is only for C instruction");
  if Batteries.String.exists p.current_line "=" then
    Batteries.String.split p.current_line ~by: "=" |> snd
  else if Batteries.String.exists p.current_line ";" then
    Batteries.String.split p.current_line ~by: ";" |> fst
  else
    raise (UnhandledOperation "command doesn't comply the format")

let jump p =
  if (command_type p) <> C_COMMAND then raise (UnhandledOperation "jump is only for C instruction");
  if Batteries.String.exists p.current_line ";" then
    Some (Batteries.String.split p.current_line ~by: ";" |> snd)
  else
    None

let symbol p =
  match command_type p with
    | A_COMMAND ->
      Batteries.String.tail p.current_line 1
    | L_COMMAND ->
      Batteries.String.strip ?chars: (Some "()") p.current_line
    | C_COMMAND ->
      raise (UnhandledOperation "dest is only for C instruction");
