open Parser

let assemble infilename outfilename =
  let outfile = open_out outfilename in

  let p = Parser.create infilename in

  while Parser.has_more_commands p do
    print_endline p.current_line;
    Parser.advance p;
  done;

  close_out outfile

let main () =
  (* TODO treat multiple files under a dir *)
  let infilename = Sys.argv.(1) in
  let outfilename = (Batteries.String.rsplit infilename "." |> fst) ^ ".asm" in
  print_endline outfilename;
  assemble infilename outfilename;;

main ()
