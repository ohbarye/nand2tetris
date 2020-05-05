type parser = {
   mutable file : in_channel;
   mutable has_next : bool;
   mutable current_line : string;
}
type command = A_COMMAND | C_COMMAND

exception NoMoreCommands of string
exception UnhandledOperation of string

module Parser : sig
  val create : in_channel -> parser
  val has_more_commands : parser -> bool
  val advance : parser -> unit
  val command_type : parser -> command
end = struct
  let has_more_commands p = p.has_next

  let command_type p =
    match String.get p.current_line 0 with
      | '@' -> A_COMMAND
      | _ -> C_COMMAND

  let advance p =
    if not (has_more_commands p) then raise (NoMoreCommands "No more commands")
    else
      try
        p.current_line <- input_line p.file;
      with End_of_file ->
        p.has_next <- false;;

  let create input =
    let p = { file = input; has_next = true; current_line = "" } in
    advance p;
    p

  let dest p =
    if (command_type p) <> C_COMMAND then raise (UnhandledOperation "dest is only for C instruction")
    else
      (* TODO *)
      "M"

  let comp p =
    if (command_type p) <> C_COMMAND then raise (UnhandledOperation "dest is only for C instruction")
    else
      (* TODO *)
      "M"

  let jump p =
    if (command_type p) <> C_COMMAND then raise (UnhandledOperation "dest is only for C instruction")
    else
      (* TODO *)
      "M"

  (* val symbol : string option -> string *)
end
