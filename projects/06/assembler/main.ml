#use "parser.ml"

let assemble filename =
  let f = open_in filename in
  let p = Parser.create f in

  while Parser.has_more_commands p do
    match Parser.command_type p with
      | C_COMMAND ->
        print_endline "C instruction";
        print_endline p.current_line;
        Parser.advance p;
      | A_COMMAND ->
        print_endline "A instruction";
        print_endline p.current_line;
        Parser.advance p;    
  done

let main () =
  let filename = Sys.argv.(1) in
  assemble filename;;

main ()
