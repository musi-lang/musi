let parse_instr_ml filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let lines = String.split_on_char '\n' content in
  let rec find_to_opcode acc = function
    | [] -> List.rev acc
    | line :: rest ->
      if String.trim line = "let to_opcode = function" then parse_cases acc rest
      else find_to_opcode acc rest
  and parse_cases acc = function
    | [] -> List.rev acc
    | line :: rest ->
      let trimmed = String.trim line in
      if trimmed = "" || not (String.starts_with ~prefix:"|" trimmed) then
        List.rev acc
      else
        let parts = String.split_on_char '>' trimmed in
        if List.length parts = 2 then
          let lhs = String.trim (List.nth parts 0) in
          let rhs = String.trim (List.nth parts 1) in
          let name_part =
            String.sub lhs 2 (String.length lhs - 2) |> String.trim
          in
          let name =
            let space_idx =
              try String.index name_part ' '
              with Not_found -> String.length name_part
            in
            String.sub name_part 0 space_idx
          in
          let opcode = String.trim rhs in
          parse_cases ((name, opcode) :: acc) rest
        else parse_cases acc rest
  in
  find_to_opcode [] lines

let generate_cpp_header opcodes =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "#pragma once\n\n";
  Buffer.add_string buf "#include <cstdint>\n\n";
  Buffer.add_string buf "namespace musi {\n\n";
  Buffer.add_string buf "  // NOLINTBEGIN(readability-identifier-naming)\n";
  Buffer.add_string buf "  enum class Opcode : uint8_t {\n";
  List.iter
    (fun (name, value) ->
      Buffer.add_string buf (Printf.sprintf "    %s = %s,\n" name value))
    opcodes;
  Buffer.add_string buf "  };\n";
  Buffer.add_string buf "  // NOLINTEND(readability-identifier-naming)\n\n";
  Buffer.add_string buf "}  // namespace musi\n";
  Buffer.contents buf

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: %s <instr.ml> <output.hpp>\n" Sys.argv.(0);
    exit 1);
  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  let opcodes = parse_instr_ml input_file in
  let header = generate_cpp_header opcodes in
  let oc = open_out output_file in
  output_string oc header;
  close_out oc;
  Printf.printf
    "Generated %s from %s (%d opcodes)\n"
    output_file
    input_file
    (List.length opcodes)
