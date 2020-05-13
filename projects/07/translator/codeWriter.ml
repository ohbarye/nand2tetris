open Parser

type writer = {
   mutable file : out_channel;
   mutable label_index : int;
}

module CodeWriter : sig
  val create : string -> writer
  val set_file_name : string -> writer -> unit
  val write_arithmetic : string -> writer -> unit
  val write_push_pop : command -> string -> int -> writer -> unit
  val close : writer -> unit
end = struct
  let create outfilename =
    let output = open_out outfilename in
    { file = output; label_index = 0 }

  let set_file_name outfilename w =
    let output = open_out outfilename in
    w.file <- output

  (* For `add`, `sub`, `and`, `or`
     2 pops, calculate, and save to stack *)
  let binary_operation command exp = Printf.sprintf "// %s
@SP
AM=M-1
D=M
A=A-1
%s" command exp

  (* For `not`, `neg`
     1 pop, calculate, and save to stack *)
  let unary_operation command exp = Printf.sprintf "// %s
@SP
A=M-1
%s" command exp

  (* For `eq`, `gt`, `lt`
     2 pops, compare, and save to stack *)
  let compare_operation command jump_type jump_index = Printf.sprintf "// %s
@SP
AM=M-1
D=M
A=A-1
D=M-D
@FALSE%d
D;%s
@SP
A=M-1
M=-1
@CONTINUE%d
0;JMP
(FALSE%d)
@SP
A=M-1
M=0
(CONTINUE%d)" command jump_index jump_type jump_index jump_index jump_index

  let write_arithmetic command w =
    let out = match command with
      "add" ->
        binary_operation command "M=M+D"
    | "sub" ->
        binary_operation command "M=M-D"
    | "and" ->
        binary_operation command "M=M&D"
    | "or" ->
        binary_operation command "M=M|D"
    | "eq" ->
        w.label_index <- w.label_index + 1;
        compare_operation command "JNE" w.label_index
    | "gt" ->
        w.label_index <- w.label_index + 1;
        compare_operation command "JLE" w.label_index
    | "lt" ->
        w.label_index <- w.label_index + 1;
        compare_operation command "JGE" w.label_index
    | "neg" -> 
        unary_operation command "M=-M"
    | "not" ->
        unary_operation command "M=!M"
    | _ -> "" in
    Printf.fprintf w.file "%s\n" out

  let write_push_pop command segment index w =
    let out = match (command, segment) with
      (C_PUSH, "constant") ->
        Printf.sprintf "// push constant %d
@%d
D=A
@SP
A=M
M=D
@SP
M=M+1" index index
    | (C_POP, "constant") ->
     "pop constant TODO"
    | _ -> "" in
    Printf.fprintf w.file "%s\n" out

  let close w =
    close_out w.file
end
