type severity = Error | Warning | Note
type category = Lex | Parse | Sema | Codegen | Runtime

type error_code =
  (*
     E0001-E0999:
     - E0001-E0099: General
     - E0100-E0199: Number/token
     - E0200-E0299: String/literal/escape
  *)
  | Lex of string
  (*
     E1000-E1999: Parse errors
  *)
  | Parse of string
  (*
     E2000-E2999: Semantic analysis
  *)
  | Sema of string
  (*
     E3000-E3999: Code generation errors
  *)
  | Codegen of string
  (*
     E4000-E4999: Runtime errors
  *)
  | Runtime of string

type t = {
    severity : severity
  ; code : error_code option
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
}

type bag = { diags : t list; errors : int; warnings : int }

let make severity message span =
  { severity; code = None; message; span; notes = [] }

let make_with_code severity code message span =
  { severity; code = Some code; message; span; notes = [] }

let error message span = make Error message span
let warning message span = make Warning message span
let note message span = make Note message span
let error_with_code code message span = make_with_code Error code message span

let warning_with_code code message span =
  make_with_code Warning code message span

let note_with_code code message span = make_with_code Note code message span

let with_note diag message span =
  { diag with notes = (message, span) :: diag.notes }

let empty_bag = { diags = []; errors = 0; warnings = 0 }

let add bag diag =
  let errors =
    match diag.severity with Error -> bag.errors + 1 | _ -> bag.errors
  in
  let warnings =
    match diag.severity with Warning -> bag.warnings + 1 | _ -> bag.warnings
  in
  { diags = diag :: bag.diags; errors; warnings }

let has_errors bag = bag.errors > 0
let has_warnings bag = bag.warnings > 0
