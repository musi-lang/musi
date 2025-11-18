type pkg_manifest = { name : string; main : string; deps : string list }

let parse_json_field json field =
  try
    let pat = "\"" ^ field ^ "\"" in
    let start_idx = String.index json pat.[0] in
    let rec find_pat pos =
      if
        pos + String.length pat <= String.length json
        && String.sub json pos (String.length pat) = pat
      then pos
      else find_pat (pos + 1)
    in
    let pat_pos = find_pat start_idx in
    let after_pat = pat_pos + String.length pat + 1 in
    let rec skip_whitespace pos =
      if pos < String.length json && (json.[pos] = ' ' || json.[pos] = '\t')
      then skip_whitespace (pos + 1)
      else pos
    in
    let after_ws = skip_whitespace after_pat in
    let after_colon =
      if json.[after_ws] = ':' then skip_whitespace (after_ws + 1) else after_ws
    in
    let after_quote =
      if json.[after_colon] = '"' then after_colon + 1 else after_colon
    in
    let end_pos = String.index_from json after_quote '"' in
    String.sub json after_quote (end_pos - after_quote)
  with _ -> ""

let load_pkg_manifest path =
  try
    let content = In_channel.with_open_text path In_channel.input_all in
    let name = parse_json_field content "name" in
    let main = parse_json_field content "main" in
    { name; main = (if main = "" then "index.ms" else main); deps = [] }
  with _ -> { name = ""; main = "index.ms"; deps = [] }

let find_std_pkgs base_path =
  let packages_dir = Filename.concat base_path "packages" in
  try
    let entries = Sys.readdir packages_dir in
    Array.fold_left
      (fun acc entry ->
        if String.length entry > 5 && String.sub entry 0 5 = "@std-" then
          let pkg_dir = Filename.concat packages_dir entry in
          let manifest_path = Filename.concat pkg_dir "mspackage.json" in
          if Sys.file_exists manifest_path then
            let manifest = load_pkg_manifest manifest_path in
            if manifest.name <> "" then
              (manifest.name, Filename.concat pkg_dir manifest.main) :: acc
            else acc
          else acc
        else acc)
      []
      entries
  with _ -> []

let std_pkgs_cache = ref None

let get_std_pkgs base_path =
  match !std_pkgs_cache with
  | Some pkgs -> pkgs
  | None ->
    let pkgs = find_std_pkgs base_path in
    std_pkgs_cache := Some pkgs;
    pkgs

let resolve_pkg name base_path =
  if String.length name >= 5 && String.sub name 0 5 = "@std/" then
    let pkgs = get_std_pkgs base_path in
    match List.find_opt (fun (pkg_name, _) -> pkg_name = name) pkgs with
    | Some (_, path) -> path
    | None -> failwith ("standard package '" ^ name ^ "' not found")
  else Filename.concat base_path (name ^ ".ms")
