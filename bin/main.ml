let usage () =
  Printf.eprintf "usage: msc <input.ms> [-o <output.msc>]\n";
  exit 1

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [] -> usage ()
  | input :: rest ->
    let output =
      match rest with
      | "-o" :: out :: _ -> out
      | _ ->
        if Filename.check_suffix input ".ms" then
          Filename.chop_suffix input ".ms" ^ ".msc"
        else input ^ ".msc"
    in
    Musi_drive.Driver.compile ~input_path:input ~output_path:output
