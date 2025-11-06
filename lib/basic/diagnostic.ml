type severity = Error | Warning | Note
type fixit = { span : Span.t; replacement : string }

type t = {
    severity : severity
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
  ; fixits : fixit list
}

type bag = { diags : t list; errors : int; warnings : int }

let make severity message span =
  { severity; message; span; notes = []; fixits = [] }

let error message span = make Error message span
let warning message span = make Warning message span
let note message span = make Note message span
let with_note t message span = { t with notes = (message, span) :: t.notes }
let with_fixit t fixit = { t with fixits = fixit :: t.fixits }
let empty_bag = { diags = []; errors = 0; warnings = 0 }
let is_empty bag = bag.diags = []
let has_errors bag = bag.errors > 0

let add bag diag =
  let errors, warnings =
    match diag.severity with
    | Error -> (bag.errors + 1, bag.warnings)
    | Warning -> (bag.errors, bag.warnings + 1)
    | Note -> (bag.errors, bag.warnings)
  in
  { diags = diag :: bag.diags; errors; warnings }

let to_list bag = List.rev bag.diags

let merge bags =
  List.fold_left
    (fun acc bag ->
      {
        diags = bag.diags @ acc.diags
      ; errors = acc.errors + bag.errors
      ; warnings = acc.warnings + bag.warnings
      })
    empty_bag
    bags

let error_count bag = bag.errors
let warning_count bag = bag.warnings

let format_severity_header = function
  | Error -> Fmt.str "%a" Fmt.(styled `Bold (styled (`Fg `Red) string)) "error:"
  | Warning ->
    Fmt.str "%a" Fmt.(styled `Bold (styled (`Fg `Yellow) string)) "warning:"
  | Note -> Fmt.str "%a" Fmt.(styled `Bold (styled (`Fg `Cyan) string)) "note:"

let caret_color_for_severity = function
  | Error -> Fmt.(styled `Bold (styled (`Fg `Red) string))
  | Warning -> Fmt.(styled `Bold (styled (`Fg `Yellow) string))
  | Note -> Fmt.(styled `Bold (styled (`Fg `Cyan) string))

let format_source_line ppf line_num text =
  Format.fprintf
    ppf
    " %s %s %s@."
    (Fmt.str "%a" Fmt.(styled `Bold string) (string_of_int line_num))
    (Fmt.str "%a" Fmt.(styled `Bold string) "|")
    text

let format_caret ppf line_num col len color_fn =
  Format.fprintf
    ppf
    " %s %s %s%s@."
    (Fmt.str
       "%a"
       Fmt.(styled `Bold string)
       (String.make (String.length (string_of_int line_num)) ' '))
    (Fmt.str "%a" Fmt.(styled `Bold string) "|")
    (String.make (col - 1) ' ')
    (Fmt.str "%a" color_fn (String.make len '^'))

let format_fixit ppf line_num col replacement =
  Format.fprintf
    ppf
    " %s %s %s%s@."
    (Fmt.str
       "%a"
       Fmt.(styled `Bold string)
       (String.make (String.length (string_of_int line_num)) ' '))
    (Fmt.str "%a" Fmt.(styled `Bold string) "|")
    (String.make (col - 1) ' ')
    (Fmt.str "%a" Fmt.(styled `Bold (styled (`Fg `Green) string)) replacement)

let emit ppf diag files =
  let severity_header = format_severity_header diag.severity in
  match Source.get_file files (Span.file diag.span) with
  | None -> Format.fprintf ppf "%s %s@." severity_header diag.message
  | Some file ->
    let line, col = Source.line_col file (Span.start diag.span) in
    Format.fprintf
      ppf
      "%s:%d:%d: %s %s@."
      (Fmt.str "%a" Fmt.(styled `Bold string) (Source.path file))
      line
      col
      severity_header
      (Fmt.str "%a" Fmt.(styled `Bold string) diag.message);
    (match Source.line_text file line with
    | None -> ()
    | Some text ->
      format_source_line ppf line text;
      let end_line, end_col = Source.line_col file (Span.end_ diag.span) in
      let len =
        if line = end_line then max 1 (end_col - col)
        else max 1 (String.length text - col + 1)
      in
      let caret_color =
        if diag.fixits = [] then caret_color_for_severity diag.severity
        else Fmt.(styled `Bold (styled (`Fg `Green) string))
      in
      format_caret ppf line col len caret_color;
      List.iter
        (fun fixit -> format_fixit ppf line col fixit.replacement)
        (List.rev diag.fixits));
    List.iter
      (fun (msg, span) ->
        let note_header =
          Fmt.str "%a" Fmt.(styled `Bold (styled (`Fg `Cyan) string)) "note:"
        in
        match Source.get_file files (Span.file span) with
        | None -> Format.fprintf ppf "%s %s@." note_header msg
        | Some f ->
          let l, c = Source.line_col f (Span.start span) in
          Format.fprintf
            ppf
            "%s:%d:%d: %s %s@."
            (Fmt.str "%a" Fmt.(styled `Bold string) (Source.path f))
            l
            c
            note_header
            msg)
      (List.rev diag.notes)

let emit_all ppf bag files = List.iter (fun d -> emit ppf d files) (to_list bag)
