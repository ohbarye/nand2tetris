let compile filepath =
  let tokenizer = JackTokenizer.create filepath in
  let outfile = (Batteries.String.rsplit filepath ~by: "." |> fst) ^ ".impl.xml" |> open_out in

  while JackTokenizer.has_more_tokens tokenizer do
    JackTokenizer.advance tokenizer
  done;

  close_out outfile;
