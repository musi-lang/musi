open Musi_basic
open Musi_parse

let has_attrib interner attribs name =
  List.exists (fun a -> Interner.lookup interner a.Node.aname = name) attribs

let get_attrib_param interner attribs attr_name param_name =
  let attrib =
    List.find_opt
      (fun a -> Interner.lookup interner a.Node.aname = attr_name)
      attribs
  in
  match attrib with
  | Some a -> (
    let param =
      List.find_opt
        (fun (k, _) -> Interner.lookup interner k = param_name)
        a.Node.aparams
    in
    match param with
    | Some (_, v) -> Some (Interner.lookup interner v)
    | None -> None)
  | None -> None

let get_attrib_arg interner attribs attr_name =
  let attrib =
    List.find_opt
      (fun a -> Interner.lookup interner a.Node.aname = attr_name)
      attribs
  in
  match attrib with
  | Some a -> (
    match a.Node.aargs with
    | arg :: _ -> Some (Interner.lookup interner arg)
    | [] -> None)
  | None -> None

type ffi_info = {
    original_name : string
  ; actual_name : string
  ; is_no_mangle : bool
  ; link_kind : string option
}

let process_ffi_attribs interner fn_name attribs is_extern =
  let original_name = Interner.lookup interner fn_name in
  if not is_extern then
    {
      original_name
    ; actual_name = original_name
    ; is_no_mangle = false
    ; link_kind = None
    }
  else
    let is_no_mangle = has_attrib interner attribs "no_mangle" in
    let link_kind = get_attrib_param interner attribs "link" "kind" in

    let actual_name =
      if is_no_mangle then original_name
      else
        match get_attrib_param interner attribs "link" "name" with
        | Some name -> name
        | None -> (
          match get_attrib_param interner attribs "link" "prefix" with
          | Some prefix -> prefix ^ original_name
          | None -> original_name)
    in
    { original_name; actual_name; is_no_mangle; link_kind }

type opt_info = {
    is_inline : bool
  ; is_no_inline : bool
  ; is_cold : bool
  ; is_must_use : bool
}

let process_opt_attribs interner attribs =
  {
    is_inline = has_attrib interner attribs "inline"
  ; is_no_inline = has_attrib interner attribs "no_inline"
  ; is_cold = has_attrib interner attribs "cold"
  ; is_must_use = has_attrib interner attribs "must_use"
  }

type layout_info = {
    is_packed : bool
  ; alignment : int option
  ; repr : string option
}

let process_layout_attribs interner attribs =
  let is_packed = has_attrib interner attribs "packed" in
  let alignment =
    match get_attrib_arg interner attribs "align" with
    | Some s -> ( try Some (int_of_string s) with _ -> None)
    | None -> None
  in
  let repr = get_attrib_arg interner attribs "repr" in
  { is_packed; alignment; repr }

type directive_info = { deprecated_msg : string option }

let process_directive_attribs interner attribs =
  { deprecated_msg = get_attrib_arg interner attribs "depr" }

type attrib_info = {
    ffi : ffi_info
  ; opt : opt_info
  ; layout : layout_info
  ; directive : directive_info
}

let process_all_attribs interner name attribs is_extern =
  {
    ffi = process_ffi_attribs interner name attribs is_extern
  ; opt = process_opt_attribs interner attribs
  ; layout = process_layout_attribs interner attribs
  ; directive = process_directive_attribs interner attribs
  }
