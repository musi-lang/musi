open Basic
open Alcotest

let test_interner () =
  let t = Interner.create () in

  let id1 = Interner.intern t "hello" in
  let _id2 = Interner.intern t "world" in
  let id3 = Interner.intern t "hello" in

  check int "same string returns same id" id1 id3;
  check
    (option string)
    "lookup existing string"
    (Interner.lookup_opt t id1)
    (Some "hello");
  check
    (option string)
    "lookup non-existent string"
    (Interner.lookup_opt t 999)
    None;
  check int "size increases with unique strings" (Interner.size t) 2;

  Interner.clear t;
  check int "size after clear" (Interner.size t) 0;
  check (option string) "lookup after clear" (Interner.lookup_opt t id1) None

let test_span () =
  let span1 = Span.make 1 10 20 in
  let span2 = Span.make 1 15 25 in
  let merged = Span.merge span1 span2 in

  check int "span file" (Span.file span1) 1;
  check int "span start" (Span.start span1) 10;
  check int "span end" (Span.end_ span1) 20;
  check int "span length" (Span.len span1) 10;

  check int "merged span start" (Span.start merged) 10;
  check int "merged span end" (Span.end_ merged) 25;
  check int "merged span file" (Span.file merged) 1

let test_source () =
  let content = "line1\nline2\nline3" in
  let source = Source.create "test.txt" content in

  check string "source path" (Source.path source) "test.txt";
  check string "source text" (Source.text source) content;

  let line1, col1 = Source.line_col source 0 in
  let line2, col2 = Source.line_col source 6 in
  let line3, col3 = Source.line_col source 12 in

  check (pair int int) "position 0 -> line 1, col 1" (line1, col1) (1, 1);
  check (pair int int) "position 6 -> line 2, col 1" (line2, col2) (2, 1);
  check (pair int int) "position 12 -> line 3, col 1" (line3, col3) (3, 1);

  check
    (option string)
    "line 1 text"
    (Source.line_text_opt source 1)
    (Some "line1");
  check
    (option string)
    "line 2 text"
    (Source.line_text_opt source 2)
    (Some "line2");
  check
    (option string)
    "non-existent line"
    (Source.line_text_opt source 99)
    None

let test_reporter () =
  let span = Span.make 1 0 5 in

  let error_diag = Reporter.error "test error" span in
  let warning_diag = Reporter.warning "test warning" span in
  let note_diag = Reporter.note "test note" span in

  let level_to_string = function
    | Reporter.Error -> "Error"
    | Reporter.Warning -> "Warning"
    | Reporter.Note -> "Note"
  in

  check string "error level" (level_to_string error_diag.Reporter.level) "Error";
  check
    string
    "warning level"
    (level_to_string warning_diag.Reporter.level)
    "Warning";
  check string "note level" (level_to_string note_diag.Reporter.level) "Note";

  let empty_bag = Reporter.empty_bag in
  check bool "empty bag is empty" (Reporter.is_empty empty_bag) true;
  check bool "empty bag has no errors" (Reporter.has_errors empty_bag) false;
  check int "empty bag error count" (Reporter.error_count empty_bag) 0;
  check int "empty bag warning count" (Reporter.warning_count empty_bag) 0;

  let bag1 = Reporter.add empty_bag error_diag in
  let bag2 = Reporter.add bag1 warning_diag in
  let bag3 = Reporter.add bag2 note_diag in

  check bool "bag with diags is not empty" (Reporter.is_empty bag3) false;
  check bool "bag with error has errors" (Reporter.has_errors bag3) true;
  check int "bag error count" (Reporter.error_count bag3) 1;
  check int "bag warning count" (Reporter.warning_count bag3) 1;
  check int "bag diags count" (List.length (Reporter.to_list bag3)) 3;

  let merged = Reporter.merge [ empty_bag; bag1; bag2 ] in
  check int "merged error count" (Reporter.error_count merged) 1;
  check int "merged warning count" (Reporter.warning_count merged) 1;

  let noted = Reporter.with_note error_diag "additional info" span in
  check
    int
    "diagnostic with note has notes"
    (List.length noted.Reporter.notes)
    1

let test_result_operations () =
  let span = Span.dummy in

  let ok_result = Reporter.try_ok 42 in
  check
    (option int)
    "try_ok returns Ok"
    (Stdlib.Result.to_option ok_result)
    (Some 42);

  let mapped = Reporter.try_map (( * ) 2) ok_result in
  check (option int) "try_map on Ok" (Stdlib.Result.to_option mapped) (Some 84);

  let error_result = Reporter.try_error_info "test error" span in
  check
    bool
    "try_error_info returns Error"
    (Stdlib.Result.is_error error_result)
    true;

  let error_mapped = Reporter.try_map (( * ) 2) error_result in
  check
    bool
    "try_map on Error stays Error"
    (Stdlib.Result.is_error error_mapped)
    true;

  let binder x = Reporter.try_ok (x + 1) in
  let bound_ok = Reporter.try_bind binder ok_result in
  check
    (option int)
    "try_bind on Ok"
    (Stdlib.Result.to_option bound_ok)
    (Some 43);

  let bound_error = Reporter.try_bind binder error_result in
  check
    bool
    "try_bind on Error stays Error"
    (Stdlib.Result.is_error bound_error)
    true

let test_suite =
  [
    ("interner", [ test_case "basic operations" `Quick test_interner ])
  ; ("span", [ test_case "creation and merge" `Quick test_span ])
  ; ("source", [ test_case "creation and lookup" `Quick test_source ])
  ; ("reporter", [ test_case "diagnostics and bags" `Quick test_reporter ])
  ; ("result", [ test_case "result operations" `Quick test_result_operations ])
  ]

let () = run "basic_tests" test_suite
