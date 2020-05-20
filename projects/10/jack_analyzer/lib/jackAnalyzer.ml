exception InvalidArgument of string

let compile filepath =
  print_endline (Printf.sprintf "Compiling %s" filepath);
  CompilerEngine.compile filepath;
  print_endline (Printf.sprintf "Finished compiling %s" filepath)

let run source =
  let is_jack source = Batteries.String.ends_with source ".jack" in
  if is_jack source then
    compile source
  else if Sys.is_directory source then
    Sys.readdir source
      |> Array.to_list
      |> List.filter is_jack
      |> List.map (fun name -> source ^ "/" ^ name)
      |> List.iter compile
  else
    raise (InvalidArgument "Argument is neither file nor directory")
