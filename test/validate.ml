let check_bytecode ~expected ~actual =
  if String.trim expected <> String.trim actual then
    failwith
      (Printf.sprintf
         "bytecode mismatch:\nexpected:\n%s\nactual:\n%s"
         expected
         actual)

let check_no_errors ~diags =
  if Diagnostic.has_errors diags then
    let messages =
      List.map
        (fun d ->
          Printf.sprintf
            "%s at %d:%d"
            d.Diagnostic.message
            (Span.start d.Diagnostic.span)
            (Span.end_ d.Diagnostic.span))
        (Diagnostic.to_list diags)
    in
    failwith
      (Printf.sprintf "expected no errors:\n%s" (String.concat "\n" messages))

let check_has_errors ~diags =
  if not (Diagnostic.has_errors diags) then failwith "expected errors, got none"
