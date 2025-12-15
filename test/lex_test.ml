open Basic
open Lex
open Alcotest

let make_lexer input =
  let source = Source.create "test.ms" input in
  Lexer.create ~interner:None source 0

let token_to_string token =
  match token with
  | Token.Ident _ -> "Ident"
  | Token.LitInt _ -> "LitInt"
  | Token.LitReal _ -> "LitReal"
  | Token.LitString _ -> "LitString"
  | Token.LitRune _ -> "LitRune"
  | Token.LitTemplateNoSubst _ -> "LitTemplateNoSubst"
  | Token.TemplateHead _ -> "TemplateHead"
  | Token.TemplateMiddle _ -> "TemplateMiddle"
  | Token.TemplateTail _ -> "TemplateTail"
  | Token.KwAnd -> "KwAnd"
  | Token.KwAs -> "KwAs"
  | Token.KwBreak -> "KwBreak"
  | Token.KwCase -> "KwCase"
  | Token.KwCycle -> "KwCycle"
  | Token.KwDefer -> "KwDefer"
  | Token.KwElse -> "KwElse"
  | Token.KwExtern -> "KwExtern"
  | Token.KwFalse -> "KwFalse"
  | Token.KwFn -> "KwFn"
  | Token.KwFor -> "KwFor"
  | Token.KwIf -> "KwIf"
  | Token.KwImport -> "KwImport"
  | Token.KwIn -> "KwIn"
  | Token.KwIs -> "KwIs"
  | Token.KwMatch -> "KwMatch"
  | Token.KwNot -> "KwNot"
  | Token.KwOr -> "KwOr"
  | Token.KwRecord -> "KwRecord"
  | Token.KwReturn -> "KwReturn"
  | Token.KwSum -> "KwSum"
  | Token.KwTrue -> "KwTrue"
  | Token.KwUnsafe -> "KwUnsafe"
  | Token.KwVal -> "KwVal"
  | Token.KwVar -> "KwVar"
  | Token.KwWhile -> "KwWhile"
  | Token.KwWith -> "KwWith"
  | Token.LBrace -> "LBrace"
  | Token.RBrace -> "RBrace"
  | Token.LBrack -> "LBrack"
  | Token.RBrack -> "RBrack"
  | Token.LBrackLt -> "LBrackLt"
  | Token.GtRBrack -> "GtRBrack"
  | Token.LParen -> "LParen"
  | Token.RParen -> "RParen"
  | Token.Comma -> "Comma"
  | Token.Dot -> "Dot"
  | Token.Colon -> "Colon"
  | Token.Semicolon -> "Semicolon"
  | Token.Eq -> "Eq"
  | Token.SlashEq -> "SlashEq"
  | Token.Lt -> "Lt"
  | Token.LtEq -> "LtEq"
  | Token.Gt -> "Gt"
  | Token.GtEq -> "GtEq"
  | Token.Plus -> "Plus"
  | Token.Minus -> "Minus"
  | Token.Star -> "Star"
  | Token.Slash -> "Slash"
  | Token.StarStar -> "StarStar"
  | Token.Amp -> "Amp"
  | Token.Bar -> "Bar"
  | Token.BarGt -> "BarGt"
  | Token.Caret -> "Caret"
  | Token.Tilde -> "Tilde"
  | Token.At -> "At"
  | Token.ColonColon -> "ColonColon"
  | Token.QuestionQuestion -> "QuestionQuestion"
  | Token.DotDot -> "DotDot"
  | Token.DotDotLt -> "DotDotLt"
  | Token.MinusGt -> "MinusGt"
  | Token.LtMinus -> "LtMinus"
  | Token.ColonEq -> "ColonEq"
  | Token.EqGt -> "EqGt"
  | Token.Question -> "Question"
  | Token.Underscore -> "Underscore"
  | Token.Dollar -> "Dollar"
  | Token.EOF -> "EOF"
  | Token.Unknown _ -> "Unknown"

let test_keywords () =
  let keywords =
    [
      ("fn", Token.KwFn)
    ; ("val", Token.KwVal)
    ; ("var", Token.KwVar)
    ; ("if", Token.KwIf)
    ; ("else", Token.KwElse)
    ; ("for", Token.KwFor)
    ; ("while", Token.KwWhile)
    ; ("match", Token.KwMatch)
    ; ("case", Token.KwCase)
    ; ("return", Token.KwReturn)
    ; ("break", Token.KwBreak)
    ; ("cycle", Token.KwCycle)
    ; ("defer", Token.KwDefer)
    ; ("import", Token.KwImport)
    ; ("extern", Token.KwExtern)
    ; ("unsafe", Token.KwUnsafe)
    ; ("record", Token.KwRecord)
    ; ("sum", Token.KwSum)
    ; ("and", Token.KwAnd)
    ; ("or", Token.KwOr)
    ; ("not", Token.KwNot)
    ; ("in", Token.KwIn)
    ; ("as", Token.KwAs)
    ; ("is", Token.KwIs)
    ; ("with", Token.KwWith)
    ; ("true", Token.KwTrue)
    ; ("false", Token.KwFalse)
    ]
  in
  List.iter
    (fun (input, expected) ->
      let lexer = make_lexer input in
      match Lexer.try_next_token lexer with
      | Ok (actual, _) ->
        if actual <> expected then
          fail
            (Printf.sprintf
               "expected %s, got %s for input: %s"
               (token_to_string expected)
               (token_to_string actual)
               input)
      | Error _ -> fail (Printf.sprintf "failed to lex keyword: %s" input))
    keywords

let test_identifiers () =
  let test_case name input =
    let lexer = make_lexer input in
    match Lexer.try_next_token lexer with
    | Ok (Token.Ident _, _) -> ()
    | Ok (token, _) ->
      fail
        (Printf.sprintf
           "%s: expected Ident, got %s"
           name
           (token_to_string token))
    | Error _ -> fail (Printf.sprintf "%s: failed to lex identifier" name)
  in
  test_case "simple identifier" "x";
  test_case "identifier with underscore" "my_var";
  test_case "identifier starting with underscore" "_private";
  test_case "camel case" "myIdentifier";
  test_case "escaped identifier" "`escaped ident`"

let test_literals () =
  let test_int_literal input =
    let lexer = make_lexer input in
    match Lexer.try_next_token lexer with
    | Ok (Token.LitInt _, _) -> ()
    | Ok (token, _) ->
      fail (Printf.sprintf "expected LitInt, got %s" (token_to_string token))
    | Error _ -> fail "failed to lex int literal"
  in

  let test_real_literal input =
    let lexer = make_lexer input in
    match Lexer.try_next_token lexer with
    | Ok (Token.LitReal _, _) -> ()
    | Ok (token, _) ->
      fail (Printf.sprintf "expected LitReal, got %s" (token_to_string token))
    | Error _ -> fail "failed to lex real literal"
  in

  let test_string_literal input =
    let lexer = make_lexer input in
    match Lexer.try_next_token lexer with
    | Ok (Token.LitString _, _) -> ()
    | Ok (token, _) ->
      fail (Printf.sprintf "expected LitString, got %s" (token_to_string token))
    | Error _ -> fail "failed to lex string literal"
  in

  let test_rune_literal input =
    let lexer = make_lexer input in
    match Lexer.try_next_token lexer with
    | Ok (Token.LitRune _, _) -> ()
    | Ok (token, _) ->
      fail (Printf.sprintf "expected LitRune, got %s" (token_to_string token))
    | Error _ -> fail "failed to lex rune literal"
  in

  test_int_literal "42";
  test_int_literal "0x2A";
  test_int_literal "0o52";
  test_int_literal "0b101010";
  test_int_literal "1_234_567";

  test_real_literal "3.14";
  test_real_literal "2.7e-3";
  test_real_literal "1_234.56_789";

  test_string_literal "\"hello\"";
  test_string_literal "\"\"";
  test_string_literal "\"with\\nescape\"";

  test_rune_literal "'a'";
  test_rune_literal "'\\n'"

let test_operators () =
  let operators =
    [
      ("+", Token.Plus)
    ; ("-", Token.Minus)
    ; ("*", Token.Star)
    ; ("/", Token.Slash)
    ; ("**", Token.StarStar)
    ; ("=", Token.Eq)
    ; ("/=", Token.SlashEq)
    ; ("<", Token.Lt)
    ; ("<=", Token.LtEq)
    ; (">", Token.Gt)
    ; (">=", Token.GtEq)
    ; ("::", Token.ColonColon)
    ; ("??", Token.QuestionQuestion)
    ; ("..", Token.DotDot)
    ; ("..<", Token.DotDotLt)
    ; ("&", Token.Amp)
    ; ("|", Token.Bar)
    ; ("^", Token.Caret)
    ; ("|>", Token.BarGt)
    ; ("->", Token.MinusGt)
    ; ("<-", Token.LtMinus)
    ; (":=", Token.ColonEq)
    ; ("=>", Token.EqGt)
    ]
  in
  List.iter
    (fun (input, expected) ->
      let lexer = make_lexer input in
      match Lexer.try_next_token lexer with
      | Ok (actual, _) ->
        if actual <> expected then
          fail
            (Printf.sprintf
               "expected %s, got %s for input: %s"
               (token_to_string expected)
               (token_to_string actual)
               input)
      | Error _ -> fail (Printf.sprintf "failed to lex operator: %s" input))
    operators

let test_delimiters () =
  let delimiters =
    [
      ("(", Token.LParen)
    ; (")", Token.RParen)
    ; ("{", Token.LBrace)
    ; ("}", Token.RBrace)
    ; ("[", Token.LBrack)
    ; ("]", Token.RBrack)
    ; ("[<", Token.LBrackLt)
    ; (">]", Token.GtRBrack)
    ; (",", Token.Comma)
    ; (".", Token.Dot)
    ; (":", Token.Colon)
    ; (";", Token.Semicolon)
    ; ("?", Token.Question)
    ; ("_", Token.Underscore)
    ; ("$", Token.Dollar)
    ; ("@", Token.At)
    ; ("~", Token.Tilde)
    ]
  in
  List.iter
    (fun (input, expected) ->
      let lexer = make_lexer input in
      match Lexer.try_next_token lexer with
      | Ok (actual, _) ->
        if actual <> expected then
          fail
            (Printf.sprintf
               "expected %s, got %s for input: %s"
               (token_to_string expected)
               (token_to_string actual)
               input)
      | Error _ -> fail (Printf.sprintf "failed to lex delimiter: %s" input))
    delimiters

let test_comments () =
  let test_line_comment () =
    let lexer = make_lexer "// this is a comment\nfn" in
    match Lexer.try_next_token lexer with
    | Ok (Token.KwFn, _) -> ()
    | Ok (token, _) ->
      fail
        (Printf.sprintf
           "expected KwFn after comment, got %s"
           (token_to_string token))
    | Error _ -> fail "failed to skip line comment"
  in

  let test_block_comment () =
    let lexer = make_lexer "/* block comment */ fn" in
    match Lexer.try_next_token lexer with
    | Ok (Token.KwFn, _) -> ()
    | Ok (token, _) ->
      fail
        (Printf.sprintf
           "expected KwFn after block comment, got %s"
           (token_to_string token))
    | Error _ -> fail "failed to skip block comment"
  in

  let test_nested_block_comment () =
    let lexer = make_lexer "/* outer /* inner */ comment */ fn" in
    match Lexer.try_next_token lexer with
    | Ok (Token.KwFn, _) -> ()
    | Ok (token, _) ->
      fail
        (Printf.sprintf
           "expected KwFn after nested block comment, got %s"
           (token_to_string token))
    | Error _ -> fail "failed to skip nested block comment"
  in

  test_line_comment ();
  test_block_comment ();
  test_nested_block_comment ()

let test_whitespace () =
  let test_whitespace_handling input =
    let lexer = make_lexer input in
    match Lexer.try_next_token lexer with
    | Ok (Token.KwFn, _) -> ()
    | Ok (token, _) ->
      fail
        (Printf.sprintf
           "expected KwFn after whitespace, got %s"
           (token_to_string token))
    | Error _ -> fail "failed to skip whitespace"
  in

  test_whitespace_handling "   fn";
  test_whitespace_handling "\tfn";
  test_whitespace_handling "\nfn";
  test_whitespace_handling "\r\nfn";
  test_whitespace_handling " \t\n\r fn"

let test_template_literals () =
  let test_template_no_subst () =
    let lexer = make_lexer "$\"hello world\"" in
    match Lexer.try_next_token lexer with
    | Ok (Token.LitTemplateNoSubst _, _) -> ()
    | Ok (token, _) ->
      fail
        (Printf.sprintf
           "expected LitTemplateNoSubst, got %s"
           (token_to_string token))
    | Error _ -> fail "failed to lex template literal"
  in

  let test_template_with_expr () =
    let lexer = make_lexer "$\"hello {name}\"" in
    match Lexer.try_next_token lexer with
    | Ok (Token.TemplateHead _, _) -> ()
    | Ok (token, _) ->
      fail
        (Printf.sprintf "expected TemplateHead, got %s" (token_to_string token))
    | Error _ -> fail "failed to lex template with expression"
  in

  test_template_no_subst ();
  test_template_with_expr ()

let test_error_handling () =
  let test_unterminated_string () =
    let lexer = make_lexer "\"unterminated" in
    match Lexer.try_next_token lexer with
    | Error _ -> ()
    | Ok (token, _) ->
      fail
        (Printf.sprintf
           "expected error for unterminated string, got %s"
           (token_to_string token))
  in

  let test_unterminated_rune () =
    let lexer = make_lexer "'a" in
    match Lexer.try_next_token lexer with
    | Error _ -> ()
    | Ok (token, _) ->
      fail
        (Printf.sprintf
           "expected error for unterminated rune, got %s"
           (token_to_string token))
  in

  let test_invalid_number () =
    let lexer = make_lexer "0x" in
    match Lexer.try_next_token lexer with
    | Error _ -> ()
    | Ok (token, _) ->
      fail
        (Printf.sprintf
           "expected error for invalid number, got %s"
           (token_to_string token))
  in

  test_unterminated_string ();
  test_unterminated_rune ();
  test_invalid_number ()

let test_lexer_properties () =
  let lexer = make_lexer "" in

  check int "initial current position" (Lexer.curr_pos lexer) 0;
  check string "source path" (Source.path (Lexer.source lexer)) "test.ms";
  check int "file id" (Lexer.file_id lexer) 0;
  check bool "no errors initially" (Lexer.has_errors lexer) false

let test_token_stream () =
  let lexer = make_lexer "a" in
  match Lexer.token_stream_opt lexer with
  | Some stream ->
    let tokens = List.of_seq (Seq.map fst stream) in
    check int "token count" (List.length tokens) 2;
    let first_token = List.nth tokens 0 in
    check
      string
      "1st token type"
      (match first_token with
      | Token.Ident _ -> "Ident"
      | _ -> token_to_string first_token)
      "Ident";
    let second_token = List.nth tokens 1 in
    check
      string
      "2nd token type"
      (match second_token with
      | Token.EOF -> "EOF"
      | _ -> token_to_string second_token)
      "EOF"
  | None -> fail "token stream should not be None"

let test_suite =
  [
    ("keywords", [ test_case "all keywords" `Quick test_keywords ])
  ; ("identifiers", [ test_case "various identifiers" `Quick test_identifiers ])
  ; ("literals", [ test_case "numeric literals" `Quick test_literals ])
  ; ("operators", [ test_case "all operators" `Quick test_operators ])
  ; ("delimiters", [ test_case "all delimiters" `Quick test_delimiters ])
  ; ("comments", [ test_case "comment handling" `Quick test_comments ])
  ; ("whitespace", [ test_case "whitespace skipping" `Quick test_whitespace ])
  ; ( "templates"
    , [ test_case "template literals" `Quick test_template_literals ] )
  ; ("errors", [ test_case "error handling" `Quick test_error_handling ])
  ; ("properties", [ test_case "lexer properties" `Quick test_lexer_properties ])
  ; ("stream", [ test_case "token stream" `Quick test_token_stream ])
  ]

let () = run "lexer_tests" test_suite
