open Parser

type writer = {
   mutable filename : string;
   mutable file : out_channel;
   mutable label_index : int;
   mutable current_function_name : string;
}

exception UnhandledOperation of string

module CodeWriter : sig
  val create : string -> writer
  val write_arithmetic : arithmetic_command -> writer -> unit
  val write_push_pop : command -> string -> int -> writer -> unit
  val write_label : string -> writer -> unit
  val write_goto : string -> writer -> unit
  val write_if : string -> writer -> unit
  val write_function : string -> int -> writer -> unit
  val write_return : writer -> unit
  val close : writer -> unit
end = struct
  let create outfilename =
    let output = open_out outfilename in
    { filename = outfilename; file = output; label_index = 0; current_function_name = "" }

  (* For `add`, `sub`, `and`, `or`
     2 pops, calculate, and save to stack *)
  let binary_operation exp = Printf.sprintf "
@SP
AM=M-1
D=M
A=A-1
%s" exp

  (* For `not`, `neg`
     1 pop, calculate, and save to stack *)
  let unary_operation exp = Printf.sprintf "
@SP
A=M-1
%s" exp

  (* For `eq`, `gt`, `lt`
     2 pops, compare, and save to stack *)
  let compare_operation jump_type jump_index = Printf.sprintf "
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
(CONTINUE%d)" jump_index jump_type jump_index jump_index jump_index

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

  let comparison_operator = function
      EQ -> "JNE"
    | GT -> "JLE"
    | LT -> "JGE"

  let binary_operator = function
      ADD -> "+"
    | SUB -> "-"
    | AND -> "&"
    | OR  -> "|"

  let unary_operator = function
      NEG -> "-"
    | NOT -> "!"

  let write_arithmetic command w =
    let out = match command with
      ADD | SUB | AND | OR ->
        binary_operation "M=M" ^ (binary_operator command) ^ "D"
    | EQ | GT | LT ->
        w.label_index <- w.label_index + 1;
        compare_operation (comparison_operator command) w.label_index
    | NEG | NOT ->
        let operator = match command with NEG -> "-" | NOT -> "!" in
        unary_operation "M=" ^ operator ^ "M" in
    Printf.fprintf w.file "%s\n" out

  let symbol_of_segment = function
      "local" -> "LCL"
    | "argument" -> "ARG"
    | "this" -> "THIS"
    | "that" -> "THAT"
    | _ -> raise (UnhandledOperation "this method is not for the command type")

  let push_operation segment index w =
    match segment with
      "constant" ->
        Printf.sprintf "// push %s %d
@%d
D=A
@SP
A=M
M=D
@SP
M=M+1" segment index index
    | "local" | "argument" | "this" | "that" ->
        indirect_push_operation (symbol_of_segment segment) index
    | "temp" ->
        indirect_push_operation "R5" (index + 5)
    | "pointer" ->
        direct_push_operation index
    | "static" ->
        direct_static_push_operation w.filename index
    | _ ->
        raise (UnhandledOperation "this method is not for the command type")

  let pop_operation segment index w =
    match segment with
        "local" | "argument" | "this" | "that" ->
          indirect_pop_operation (symbol_of_segment segment) index
      | "temp" ->
          indirect_pop_operation "R5" (index + 5)
      | "pointer" ->
          direct_pop_operation index
      | "static" ->
          direct_static_pop_operation w.filename index
      | _ ->
          raise (UnhandledOperation "this method is not for the command type")

  let write_push_pop command segment index w =
    let out = match command with
      C_PUSH -> push_operation segment index w
    | C_POP -> pop_operation segment index w
    | _ -> raise (UnhandledOperation "this method is not for the command type") in
    Printf.fprintf w.file "%s\n" out

  let full_label label w =
    if Batteries.String.is_empty w.current_function_name then
      label
    else
      w.current_function_name ^ "$" ^ label

  let write_label label w =
    "(" ^ (full_label label w) ^ ")"
      |> Printf.fprintf w.file "%s\n"

  let write_goto label w =
    "@" ^ (full_label label w) ^ "\n" ^
    "0;JMP"
      |> Printf.fprintf w.file "%s\n"

  let write_if label w =
    let fl = full_label label w in
    Printf.sprintf "// if-goto %s
@SP
AM=M-1
D=M
@%s
D;JNE" fl fl
      |> Printf.fprintf w.file "%s\n"

  let write_function function_name num_locals w = Printf.sprintf "// function %s
(%s)
" function_name function_name
      |> Printf.fprintf w.file "%s\n";
    for i = 1 to num_locals do
      write_push_pop C_PUSH "constant" 0 w;
    done;
    w.current_function_name <- function_name

  let write_return w = Printf.sprintf "// return
@LCL
D=M
@R13
M=D  // R13 = FRAME = LCL
@5
D=A
@R13
A=M-D
D=M  // D = *(FRAME-5) = return-address
@R14
M=D  // R14 = return-address
@SP
M=M-1
A=M
D=M
@ARG
A=M  // M = *ARG
M=D  // *ARG = pop()

@ARG
D=M+1
@SP
M=D  // SP = ARG + 1

@R13
AM=M-1  // A = FRAME-1, R13 = FRAME-1
D=M
@THAT
M=D  // THAT = *(FRAME-1)

@R13
AM=M-1
D=M
@THIS
M=D  // THIS = *(FRAME-2)

@R13
AM=M-1
D=M
@ARG
M=D  // ARG = *(FRAME-3)

@R13
AM=M-1
D=M
@LCL
M=D  // LCL = *(FRAME-4)

@R14
A=M
0;JMP  // goto return-address"
      |> Printf.fprintf w.file "%s\n"

  let close w =
    close_out w.file
end
