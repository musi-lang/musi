open Musi_basic

type t = {
    name : Interner.name
  ; ty : Types.t ref
  ; is_mutable : bool
  ; span : Span.t
}

type scope = { parent : scope option; bindings : (Interner.name, t) Hashtbl.t }
type table = { scopes : scope list ref }

let scope_size = 16

let empty_table () =
  let global_scope = { parent = None; bindings = Hashtbl.create scope_size } in
  { scopes = ref [ global_scope ] }

let curr_scope table =
  match !(table.scopes) with
  | [] -> failwith "no active scope"
  | current :: _ -> current

let enter_scope table =
  let curr = curr_scope table in
  let new_scope =
    { parent = Some curr; bindings = Hashtbl.create scope_size }
  in
  table.scopes := new_scope :: !(table.scopes);
  table

let exit_scope table =
  match !(table.scopes) with
  | [] | [ _ ] -> failwith "cannot exit global scope"
  | _ :: rest ->
    table.scopes := rest;
    table

let bind table name sym =
  let curr = curr_scope table in
  Hashtbl.add curr.bindings name sym;
  table

let rec lookup_in_scope scope name =
  match Hashtbl.find_opt scope.bindings name with
  | Some sym -> Some sym
  | None -> (
    match scope.parent with
    | None -> None
    | Some parent -> lookup_in_scope parent name)

let lookup table name =
  match !(table.scopes) with
  | [] -> None
  | curr :: _ -> lookup_in_scope curr name

let lookup_curr table name =
  match !(table.scopes) with
  | [] -> None
  | curr :: _ -> Hashtbl.find_opt curr.bindings name
