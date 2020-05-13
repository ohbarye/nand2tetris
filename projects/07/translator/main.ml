open Parser
open CodeWriter

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

let translate infilename outfilename =
  let w = CodeWriter.create outfilename in
  let p = Parser.create infilename in
  write w p;
  CodeWriter.close w

let main () =
  (* TODO treat multiple files under a dir *)
  let infilename = Sys.argv.(1) in
  let outfilename = (Batteries.String.rsplit infilename "." |> fst) ^ ".asm" in
  translate infilename outfilename;;

main ()
