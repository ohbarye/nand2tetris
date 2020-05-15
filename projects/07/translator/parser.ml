type parser = {
   mutable file : in_channel;
   mutable has_next : bool;
   mutable current_line : string;
}
type command = C_ARITHMETIC | C_PUSH | C_POP | C_LABEL | C_GOTO | C_IF | C_FUNCTION | C_RETURN | C_CALL
type arithmetic_command = ADD | SUB | AND | OR | EQ | GT | LT | NEG | NOT

exception NoMoreCommands of string
exception UnhandledOperation of string

module Parser : sig
  val create : string -> parser
  val has_more_commands : parser -> bool
  val advance : parser -> unit
  val command_type : parser -> command
  val arithmetic_command_type : parser -> arithmetic_command
  val arg1 : parser -> string
  val arg2 : parser -> string
end = struct
  let has_more_commands p = p.has_next

  let command p =
    List.nth (Batteries.String.split_on_char ' ' p.current_line) 0

  let arithmetic_command_type p =
    match command p with
      | "add" -> ADD
      | "sub" -> SUB
      | "neg" -> NEG
      | "eq"  -> EQ
      | "gt"  -> GT
      | "lt"  -> LT
      | "and" -> AND
      | "or"  -> OR
      | "not" -> NOT
      | _ -> raise (UnhandledOperation "this command is not available yet")

  let command_type p =
    match command p with
      | "pop" -> C_POP
      | "push" -> C_PUSH
      | "add" | "sub" | "neg" | "eq" | "gt"  | "lt" | "and" | "or" | "not" -> C_ARITHMETIC
      | _ -> raise (UnhandledOperation "this command is not available yet")

  let trim str =
    if Batteries.String.exists str "//" then
      Batteries.String.split str "//"
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

  let create infilename =
    let input = open_in infilename in
    let p = { file = input; has_next = true; current_line = "" } in
    advance p;
    p

  let arg1 p =
    match command_type p with
      C_RETURN -> raise (UnhandledOperation "this method is not for the command type")
    | C_ARITHMETIC -> p.current_line
    | _ -> List.nth (Batteries.String.split_on_char ' ' p.current_line) 1

  let arg2 p =
    match command_type p with
      C_PUSH | C_POP | C_FUNCTION | C_CALL -> List.nth (Batteries.String.split_on_char ' ' p.current_line) 2
    | _ -> raise (UnhandledOperation "this method is not for the command type")
end