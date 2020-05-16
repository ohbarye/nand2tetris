open Parser
open CodeWriter

exception InvalidArgument of string

let write_command w p =
  let command = Parser.command_type p in
  match command with
      C_ARITHMETIC ->
        CodeWriter.write_arithmetic (Parser.arithmetic_command_type p) w
    | C_PUSH | C_POP ->
        let segment = Parser.arg1 p in
        let index = int_of_string (Parser.arg2 p) in
        CodeWriter.write_push_pop command segment index w
    | C_LABEL ->
        CodeWriter.write_label (Parser.arg1 p) w
    | C_GOTO ->
        CodeWriter.write_goto (Parser.arg1 p) w
    | C_IF ->
        CodeWriter.write_if (Parser.arg1 p) w
    | C_FUNCTION ->
        let function_name = Parser.arg1 p in
        let num_locals = int_of_string (Parser.arg2 p) in
        CodeWriter.write_function function_name num_locals w
    | C_RETURN ->
        CodeWriter.write_return w
    | C_CALL ->
        let function_name = Parser.arg1 p in
        let num_args = int_of_string (Parser.arg2 p) in
        CodeWriter.write_call function_name num_args w

let write w p =
  while Parser.has_more_commands p do
    write_command w p;
    Parser.advance p
  done

let translate w infilename =
  CodeWriter.set_file_name infilename w;
  let p = Parser.create infilename in
  write w p

let filename_list infilename =
  if Batteries.String.exists infilename ".vm" then
    ([infilename], ((Batteries.String.rsplit infilename "." |> fst) ^ ".asm"))
  else if Sys.is_directory infilename then
    let infilenames = Sys.readdir infilename
      |> Array.to_list
      |> List.filter (fun name -> Batteries.String.exists name ".vm")
      |> List.map (fun name -> infilename ^ "/" ^ name) in
    let filename = (Batteries.String.rsplit infilename "/" |> snd) ^ ".asm" in
    (infilenames, infilename ^ "/" ^ filename)
  else
    raise (InvalidArgument "Argument is neither file nor directory")

let main () =
  let (infilenames, outfilename) = filename_list Sys.argv.(1) in
  let w = CodeWriter.create outfilename in
  let translate = translate w in
  List.iter translate infilenames;
  CodeWriter.close w;;

main ()
