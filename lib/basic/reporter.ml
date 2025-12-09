type level = Error | Warning | Note

type t = {
    level : level
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
}

type bag = { diags : t list; errors : int; warnings : int }

let make level message span = { level; message; span; notes = [] }
let error message span = make Error message span
let warning message span = make Warning message span
let note message span = make Note message span

let add_note message span diag =
  { diag with notes = (message, span) :: diag.notes }

let empty_bag = { diags = []; errors = 0; warnings = 0 }

let add bag diag =
  let errors =
    match diag.level with Error -> bag.errors + 1 | _ -> bag.errors
  in
  let warnings =
    match diag.level with Warning -> bag.warnings + 1 | _ -> bag.warnings
  in
  { diags = diag :: bag.diags; errors; warnings }

let add_list bag diags = List.fold_left add bag diags

let merge bag1 bag2 =
  {
    diags = bag1.diags @ bag2.diags
  ; errors = bag1.errors + bag2.errors
  ; warnings = bag1.warnings + bag2.warnings
  }

let get_errors bag = List.filter (fun d -> d.level = Error) bag.diags
let get_warnings bag = List.filter (fun d -> d.level = Warning) bag.diags
let get_all bag = List.rev bag.diags
let count_errors bag = bag.errors
let count_warnings bag = bag.warnings
let is_empty bag = bag.errors = 0 && bag.warnings = 0
