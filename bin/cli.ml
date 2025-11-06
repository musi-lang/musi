type command =
  | Compile of {
        input : string
      ; output : string option
      ; search_paths : string list
    }
  | Check of { input : string; search_paths : string list }
  | Run of { input : string; search_paths : string list }
  | Disasm of { input : string }
  | Help
  | Version

let parse_args () =
  let args = Array.to_list Sys.argv in
  let rec extract_flags args output search_paths =
    match args with
    | "-o" :: out :: rest | "--output" :: out :: rest ->
      extract_flags rest (Some out) search_paths
    | "-I" :: path :: rest -> extract_flags rest output (path :: search_paths)
    | rest -> (output, List.rev search_paths, rest)
  in
  match args with
  | _ :: "compile" :: input :: rest ->
    let output, search_paths, _ = extract_flags rest None [] in
    Compile { input; output; search_paths }
  | _ :: "check" :: input :: rest ->
    let _, search_paths, _ = extract_flags rest None [] in
    Check { input; search_paths }
  | _ :: "run" :: input :: rest ->
    let _, search_paths, _ = extract_flags rest None [] in
    Run { input; search_paths }
  | _ :: "disasm" :: input :: _ -> Disasm { input }
  | _ :: ("help" | "--help" | "-h") :: _ -> Help
  | _ :: ("version" | "--version" | "-V") :: _ -> Version
  | _ :: input :: rest when String.ends_with ~suffix:".ms" input ->
    let output, search_paths, _ = extract_flags rest None [] in
    Compile { input; output; search_paths }
  | _ -> Help
