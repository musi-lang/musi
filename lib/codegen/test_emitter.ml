open Alcotest

let dummy_span = Span.make 0 0 0
let make_node kind = Node.make kind dummy_span

let test_collect_procs_empty () =
  let interner = Interner.create () in
  let ast = [] in
  let table = Emitter.collect_procs interner ast in
  check int "has main" 1 (Hashtbl.length table)

let test_collect_procs_single () =
  let interner = Interner.create () in
  let name = Interner.intern interner "foo" in
  let pat = make_node (Node.PatBinding name) in
  let proc =
    make_node
      (Node.ExprProc
         {
           modifiers =
             {
               decorators = []
             ; is_exported = false
             ; is_unsafe = false
             ; is_extern = (false, None)
             ; is_async = false
             }
         ; params = []
         ; ret_ty = None
         ; body = None
         })
  in
  let binding =
    make_node
      (Node.ExprBinding
         {
           modifiers =
             {
               decorators = []
             ; is_exported = false
             ; is_unsafe = false
             ; is_extern = (false, None)
             ; is_async = false
             }
         ; is_mutable = false
         ; pat
         ; ty = None
         ; init = proc
         })
  in
  let stmt = make_node (Node.StmtExpr binding) in
  let ast = [ stmt ] in
  let table = Emitter.collect_procs interner ast in
  check int "has main and foo" 2 (Hashtbl.length table)

let test_emit_literal_zero () =
  let interner = Interner.create () in
  let lit = make_node (Node.ExprLitNumeric ("0", None)) in
  let ast = [ make_node (Node.StmtExpr lit) ] in
  let proc_table = Emitter.collect_procs interner ast in
  let diags = ref Diagnostic.empty_bag in
  let procs = Emitter.emit_program interner proc_table diags ast in
  let procs, _module_desc = procs in
  check int "has main" 1 (Array.length procs);
  check bool "has instructions" true (List.length procs.(0) > 0)

let () =
  run
    "Emitter"
    [
      ( "collect_procs"
      , [
          test_case "empty ast" `Quick test_collect_procs_empty
        ; test_case "single procedure" `Quick test_collect_procs_single
        ] )
    ; ( "emit_program"
      , [ test_case "literal zero" `Quick test_emit_literal_zero ] )
    ]
