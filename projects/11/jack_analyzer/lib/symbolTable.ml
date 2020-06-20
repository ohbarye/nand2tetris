type identifierKind = FIELD | STATIC | VAR | ARG

type identifier = {
  identifier_type : string;
  kind : identifierKind;
  index : int;
}

module Table = Map.Make(String)

type symbolTable = {
  mutable static_table : identifier Table.t;
  mutable field_table : identifier Table.t;
  mutable arg_table : identifier Table.t;
  mutable var_table : identifier Table.t;
}

let create =
  {
    static_table = Table.empty;
    field_table = Table.empty;
    arg_table = Table.empty;
    var_table = Table.empty;
  }

let define name identifier_type kind table =
  match kind with
  | STATIC ->
    let index = Table.cardinal table.static_table in
    let id = { identifier_type = identifier_type; kind = kind; index = index } in
    table.static_table <- Table.add name id table.static_table;
    table
  | FIELD ->
    let index = Table.cardinal table.field_table in
    let id = { identifier_type = identifier_type; kind = kind; index = index } in
    table.field_table <- Table.add name id table.field_table;
    table
  | ARG ->
    let index = Table.cardinal table.arg_table in
    let id = { identifier_type = identifier_type; kind = kind; index = index } in
    table.arg_table <- Table.add name id table.arg_table;
    table
  | VAR ->
    let index = Table.cardinal table.var_table in
    let id = { identifier_type = identifier_type; kind = kind; index = index } in
    table.var_table <- Table.add name id table.var_table;
    table

let start_subroutine table =
  table.arg_table <- Table.empty;
  table.var_table <- Table.empty;
  table

let find name table =
  match name with
  | _ when Table.mem name table.static_table ->
    Some (Table.find name table.static_table)
  | _ when Table.mem name table.field_table ->
    Some (Table.find name table.field_table)
  | _ when Table.mem name table.arg_table ->
    Some (Table.find name table.arg_table)
  | _ when Table.mem name table.var_table ->
    Some (Table.find name table.var_table)
  | _ ->
    None

let kind_of name table =
  match find name table with
  | Some id -> Some (id.kind)
  | None -> None

let type_of name table =
  match find name table with
  | Some id -> Some (id.identifier_type)
  | None -> None

let index_of name table =
  match find name table with
  | Some id -> Some (id.index)
  | None -> None
