open Musi_drive

let usage () =
  Printf.eprintf "usage: msc <input.ms> [-o <output.mso>]\n";
  exit 1

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [] -> usage ()
  | input :: rest when not (String.starts_with ~prefix:"-" input) ->
    let output =
      match rest with
      | "-o" :: out :: _ -> out
      | _ ->
        if Filename.check_suffix input ".ms" then
          Filename.chop_suffix input ".ms" ^ ".mso"
        else input ^ ".mso"
    in
    Driver.compile ~input_path:input ~output_path:output
  | _ -> usage ()
