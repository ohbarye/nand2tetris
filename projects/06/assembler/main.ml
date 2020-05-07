open Parser
open Code
open SymbolTable

let zfill width s =
  let to_fill = width - (String.length s) in
  if to_fill <= 0 then s
  else (String.make to_fill '0') ^ s

let assemble infilename outfilename =
  let infile = open_in infilename in
  let outfile = open_out outfilename in
  let p = Parser.create infile in

  while Parser.has_more_commands p do
    Parser.advance p;
  done;

  seek_in infile 0;
  let p = Parser.create infile in

  while Parser.has_more_commands p do
    match Parser.command_type p with
      | C_COMMAND ->
        let comp = Code.comp (Parser.comp p) in
        let dest = Code.dest (Parser.dest p) in
        let jump = Code.jump (Parser.jump p) in
        Printf.fprintf outfile "111%s%s%s\n" comp dest jump;
        Parser.advance p;
      | A_COMMAND ->
        Batteries.String.tail p.current_line 1
          |> Batteries.Big_int.of_string
          |> Batteries.Big_int.to_string_in_binary
          |> zfill 15
          |> Printf.fprintf outfile "0%s\n";
        Parser.advance p;
  done;
  close_in infile;
  close_out outfile;;

let main () =
  let infilename = Sys.argv.(1) in
  let outfilename = Sys.argv.(2) in
  assemble infilename outfilename;;

main ()
