open Musi_basic
open Musi_parse

type mono_instance = {
    name : Interner.name
  ; ty_args : Types.t list
  ; concrete_name : Interner.name
}

type mono_state = { instances : mono_instance list ref; interner : Interner.t }

let create interner = { instances = ref []; interner }

let get_concrete_name mono_state base_name ty_args =
  let ty_suffix = String.concat "_" (List.map Types.show ty_args) in
  let concrete_name_str =
    Printf.sprintf
      "%s_%s"
      (Interner.lookup mono_state.interner base_name)
      ty_suffix
  in
  Interner.intern mono_state.interner concrete_name_str

let add_inst mono_state name ty_args =
  let concrete_name = get_concrete_name mono_state name ty_args in
  let instance = { name; ty_args; concrete_name } in
  mono_state.instances := instance :: !(mono_state.instances);
  concrete_name

let manage_generic_insting mono_state base_name ty_args lookup ?expected () =
  match expected with
  | Some expected_ty -> expected_ty
  | None -> (
    match lookup base_name with
    | Some scheme -> Types.instiate scheme
    | None ->
      let concrete_name = add_inst mono_state base_name ty_args in
      Types.TyNamed concrete_name)

let rec infer_ty mono_state lookup ty_node =
  match ty_node.Node.tkind with
  | Node.TyApp (base_ty, arg_tys) -> (
    match base_ty.tkind with
    | Node.TyNamed base_name ->
      let arg_tys = List.map (infer_ty mono_state lookup) arg_tys in
      manage_generic_insting mono_state base_name arg_tys lookup ()
    | _ -> Types.from_node lookup ty_node)
  | _ -> Types.from_node lookup ty_node
