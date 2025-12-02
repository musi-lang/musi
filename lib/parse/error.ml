open Basic

module type S = sig
  type code = E1003 | E1101 | E1102 | E1103 | E1104 | E1105 | E1108 | E1115

  val code_string : code -> string
  val diag : code -> Span.t -> string list -> Diagnostic.t
end

module Make () : S = struct
  type code = E1003 | E1101 | E1102 | E1103 | E1104 | E1105 | E1108 | E1115

  let code_string = function
    | E1003 -> "E1003"
    | E1101 -> "E1101"
    | E1102 -> "E1102"
    | E1103 -> "E1103"
    | E1104 -> "E1104"
    | E1105 -> "E1105"
    | E1108 -> "E1108"
    | E1115 -> "E1115"

  let diag code span args =
    let msg =
      match (code, args) with
      | E1003, [ expected; found ] ->
        Printf.sprintf "expected %s, found %s" expected found
      | E1101, [] -> "expected expression"
      | E1102, [] -> "expected statement"
      | E1103, [] -> "expected pattern"
      | E1104, [] -> "expected type"
      | E1105, [] -> "expected identifier"
      | E1108, [] -> "expected '{'"
      | E1115, [] -> "expected 'from'"
      | _, _ -> "unknown parse error"
    in
    Diagnostic.error_with_code (Diagnostic.Parse (code_string code)) msg span
end

include Make ()
