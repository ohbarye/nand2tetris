open Parser
open CodeWriter

exception InvalidArgument of string

let write_command w p =
  let command = Parser.command_type p in
  match command with
      C_ARITHMETIC ->
        CodeWriter.write_arithmetic (p.current_line) w
    | C_PUSH | C_POP ->
        let segment = Parser.arg1 p in
        let index = int_of_string (Parser.arg2 p) in
        CodeWriter.write_push_pop command segment index w
    | _ -> ()

let write w p =
  while Parser.has_more_commands p do
    write_command w p;
    Parser.advance p
  done

let translate infilename =
  let outfilename = (Batteries.String.rsplit infilename "." |> fst) ^ ".asm" in
  let w = CodeWriter.create outfilename in
  let p = Parser.create infilename in
  write w p;
  CodeWriter.close w

let filename_list infilename =
  if Batteries.String.exists infilename ".vm" then
    [infilename]
  else if Sys.is_directory infilename then
    Sys.readdir infilename
      |> Array.to_list
      |> List.filter (fun name -> Batteries.String.exists name ".vm")
      |> List.map (fun name -> infilename ^ "/" ^ name)
  else
    raise (InvalidArgument "Argument is neither file nor directory")

let main () =
  let infilenames = filename_list Sys.argv.(1) in
  List.iter translate infilenames;;

main ()
