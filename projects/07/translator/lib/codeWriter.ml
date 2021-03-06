open Parser

type writer = {
   mutable filename : string;
   mutable file : out_channel;
   mutable label_index : int;
}

exception UnhandledOperation of string
exception ArgumentError

let create outfilename =
  let output = open_out outfilename in
  { filename = outfilename; file = output; label_index = 0 }

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
    (Batteries.String.rsplit filename ~by: "/" |> snd)
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
  | _ -> raise ArgumentError

let binary_operator = function
    ADD -> "+"
  | SUB -> "-"
  | AND -> "&"
  | OR  -> "|"
  | _ -> raise ArgumentError

let write_arithmetic command w =
  let out = match command with
    ADD | SUB | AND | OR ->
      binary_operation "M=M" ^ (binary_operator command) ^ "D"
  | EQ | GT | LT ->
      w.label_index <- w.label_index + 1;
      compare_operation (comparison_operator command) w.label_index
  | NEG | NOT ->
      let operator = match command with
          NEG -> "-"
        | NOT -> "!"
        | _ -> raise ArgumentError in
      unary_operation operator in
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

let close w =
  close_out w.file
