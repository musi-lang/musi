open Alcotest

let test_empty () =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 "" interner in
  let tokens, diags = Lexer.lex lexer in
  check int "eof only" 1 (List.length tokens);
  check bool "no errors" false (Diagnostic.has_errors diags)

let test_underscore () =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 "_" interner in
  let tokens, _diags = Lexer.lex lexer in
  match List.hd tokens with
  | { Token.kind = Token.Underscore; _ } -> ()
  | _ -> fail "expected underscore token"

let test_keyword_vs_ident () =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 "proc procedure" interner in
  let tokens, _diags = Lexer.lex lexer in
  let kinds = List.map (fun t -> t.Token.kind) tokens in
  match kinds with
  | [ Token.KwProc; Token.Whitespace; Token.Ident _; Token.Eof ] -> ()
  | _ -> fail "expected keyword then identifier"

let test_operator_longest_match () =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 "<-" interner in
  let tokens, _diags = Lexer.lex lexer in
  match List.hd tokens with
  | { Token.kind = Token.LtMinus; _ } -> ()
  | _ -> fail "expected <- not < and -"

let () =
  run
    "Lexer"
    [
      ( "unit"
      , [
          test_case "empty" `Quick test_empty
        ; test_case "underscore" `Quick test_underscore
        ; test_case "keyword_vs_ident" `Quick test_keyword_vs_ident
        ; test_case "operator_longest_match" `Quick test_operator_longest_match
        ] )
    ]
