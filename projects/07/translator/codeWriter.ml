open Parser

type writer = {
   mutable filename : string;
   mutable file : out_channel;
   mutable label_index : int;
}

exception UnhandledOperation of string

module CodeWriter : sig
  val create : string -> writer
  val set_file_name : string -> writer -> unit
  val write_arithmetic : string -> writer -> unit
  val write_push_pop : command -> string -> int -> writer -> unit
  val close : writer -> unit
end = struct
  let create outfilename =
    let output = open_out outfilename in
    { filename = outfilename; file = output; label_index = 0 }

  let set_file_name outfilename w =
    let output = open_out outfilename in
    w.filename <- outfilename;
    w.file <- output;;

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

  let static_symbol filename index =
    let name = if Batteries.String.exists filename "/" then
      (Batteries.String.rsplit filename "/" |> snd)
    else
      filename in
    name ^ "." ^ (string_of_int index)

  (* For `local`, `argument`, `this`, `that`, `temp` segments *)
  let indirect_push_operation symbol index = Printf.sprintf "// push %s %d
@%s
D=M
@%d
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1" symbol index symbol index

  (* For `pointer` segments *)
  let direct_push_operation index =
    let symbol = match index with
      0 -> "THIS"
    | 1 -> "THAT"
    | _ -> raise (UnhandledOperation "this method is not for the command type") in
   Printf.sprintf "// push %s
@%s
D=M
@SP
A=M
M=D
@SP
M=M+1" symbol symbol

  (* For `pointer` segments *)
  let direct_push_operation index =
    let symbol = match index with
      0 -> "THIS"
    | 1 -> "THAT"
    | _ -> raise (UnhandledOperation "this method is not for the command type") in
   Printf.sprintf "// push %s
@%s
D=M
@SP
A=M
M=D
@SP
M=M+1" symbol symbol

  (* For `pointer` segments *)
  let direct_static_push_operation filename index =
    let symbol = static_symbol filename index in
    Printf.sprintf "// push %s
@%s
D=M
@SP
A=M
M=D
@SP
M=M+1" symbol symbol

  (* For `local`, `argument`, `this`, `that`, `temp` segments *)
  let indirect_pop_operation symbol index = Printf.sprintf "// pop %s %d
@%s
D=M
@%d
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D" symbol index symbol index

  (* For `pointer` segments *)
  let direct_pop_operation index =
    let symbol = match index with
        0 -> "THIS"
      | 1 -> "THAT"
      | _ -> raise (UnhandledOperation "this method is not for the command type") in
    Printf.sprintf "// pop %s
@%s
D=A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D" symbol symbol

  (* For `pointer` segments *)
  let direct_static_pop_operation filename index =
    let symbol = static_symbol filename index in
    Printf.sprintf "// pop %s
@%s
D=A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D" symbol symbol

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
    | _ -> raise (UnhandledOperation "this method is not for the command type") in
    Printf.fprintf w.file "%s\n" out

  let write_push_pop command segment index w =
    let out = match (command, segment) with
      (C_PUSH, "constant") ->
        Printf.sprintf "// push %s %d
@%d
D=A
@SP
A=M
M=D
@SP
M=M+1" segment index index
    | (C_PUSH, "local") ->
        indirect_push_operation "LCL" index
    | (C_PUSH, "argument") ->
        indirect_push_operation "ARG" index
    | (C_PUSH, "this") ->
        indirect_push_operation "THIS" index
    | (C_PUSH, "that") ->
        indirect_push_operation "THAT" index
    | (C_PUSH, "temp") ->
        indirect_push_operation "R5" (index + 5)
    | (C_PUSH, "pointer") ->
        direct_push_operation index
    | (C_PUSH, "static") ->
        direct_static_push_operation w.filename index
    | (C_POP, "local") ->
        indirect_pop_operation "LCL" index
    | (C_POP, "argument") ->
        indirect_pop_operation "ARG" index
    | (C_POP, "this") ->
        indirect_pop_operation "THIS" index
    | (C_POP, "that") ->
        indirect_pop_operation "THAT" index
    | (C_POP, "temp") ->
        indirect_pop_operation "R5" (index + 5)
    | (C_POP, "pointer") ->
        direct_pop_operation index
    | (C_POP, "static") ->
        direct_static_pop_operation w.filename index
    | _ ->
        raise (UnhandledOperation "this method is not for the command type") in
    Printf.fprintf w.file "%s\n" out

  let close w =
    close_out w.file
end
