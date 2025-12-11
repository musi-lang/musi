type level = Error | Warning | Note

type t = {
    level : level
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
}

type bag = { diags : t list; errors : int; warnings : int }
type 'a result = ('a, bag) Stdlib.result

let make level message span = { level; message; span; notes = [] }
let error message span = make Error message span
let warning message span = make Warning message span
let note message span = make Note message span

let with_note diag message span =
  { diag with notes = (message, span) :: diag.notes }

let empty_bag = { diags = []; errors = 0; warnings = 0 }
let is_empty bag = bag.diags = []
let has_errors bag = bag.errors > 0

let add bag diag =
  let errors, warnings =
    match diag.level with
    | Error -> (bag.errors + 1, bag.warnings)
    | Warning -> (bag.errors, bag.warnings + 1)
    | Note -> (bag.errors, bag.warnings)
  in
  { diags = diag :: bag.diags; errors; warnings }

let to_list bag = List.rev bag.diags

let merge bags =
  let all_diags = List.concat_map (fun b -> List.rev b.diags) bags in
  let errors = List.fold_left (fun acc b -> acc + b.errors) 0 bags in
  let warnings = List.fold_left (fun acc b -> acc + b.warnings) 0 bags in
  { diags = List.rev all_diags; errors; warnings }

let error_count bag = bag.errors
let warning_count bag = bag.warnings
let use_color () = try Unix.isatty Unix.stdout with _ -> false

let styled style ppf s =
  if use_color () then Fmt.(styled style string) ppf s
  else Format.pp_print_string ppf s

let level_header ppf = function
  | Error -> styled `Bold ppf "error:"
  | Warning -> styled `Bold ppf "warning:"
  | Note -> styled `Bold ppf "note:"

let level_color = function Error -> `Red | Warning -> `Yellow | Note -> `Cyan

let emit_loc ppf file line col =
  Format.fprintf ppf "%a:%d:%d:" (styled `Bold) (Source.path file) line col

let emit_source_line ppf line_num text =
  Format.fprintf ppf " %a | %s@." (styled `Bold) (string_of_int line_num) text

let emit_caret ppf line_num col len color =
  let padding = String.make (String.length (string_of_int line_num)) ' ' in
  let spaces = String.make (col - 1) ' ' in
  let carets = String.make len '^' in
  Format.fprintf ppf " %s | %s%a@." padding spaces (styled (`Fg color)) carets

let emit_note ppf files msg span =
  match Source.get_file_opt files (Span.file span) with
  | None ->
    level_header ppf Note;
    Format.fprintf ppf " %s@." msg
  | Some f ->
    let l, c = Source.line_col f (Span.start span) in
    emit_loc ppf f l c;
    Format.fprintf ppf " ";
    level_header ppf Note;
    Format.fprintf ppf " %s@." msg

let emit ppf diag files =
  match Source.get_file_opt files (Span.file diag.span) with
  | None ->
    level_header ppf diag.level;
    Format.fprintf ppf " %s@." diag.message
  | Some file ->
    let line, col = Source.line_col file (Span.start diag.span) in
    emit_loc ppf file line col;
    Format.fprintf ppf " ";
    level_header ppf diag.level;
    Format.fprintf ppf " %s@." diag.message;
    (match Source.line_text_opt file line with
    | None -> ()
    | Some text ->
      emit_source_line ppf line text;
      let end_line, end_col = Source.line_col file (Span.end_ diag.span) in
      let len =
        if line = end_line then max 1 (end_col - col)
        else max 1 (String.length text - col + 1)
      in
      emit_caret ppf line col len (level_color diag.level));
    List.iter
      (fun (msg, span) -> emit_note ppf files msg span)
      (List.rev diag.notes)

let emit_all ppf bag files = List.iter (fun d -> emit ppf d files) (to_list bag)
let try_ok x = Stdlib.Ok x
let try_error_bag bag = Stdlib.Error bag
let try_error_diag diag = Stdlib.Error (add empty_bag diag)
let try_error_info msg span = try_error_diag (error msg span)
let try_warning_diag diag = Stdlib.Error (add empty_bag diag)
let try_warning_info msg span = try_warning_diag (warning msg span)

let try_bind f = function
  | Stdlib.Ok x -> f x
  | Stdlib.Error bag -> Stdlib.Error bag

let try_map f = function
  | Stdlib.Ok x -> Stdlib.Ok (f x)
  | Stdlib.Error bag -> Stdlib.Error bag

let try_map_error f = function
  | Stdlib.Ok x -> Stdlib.Ok x
  | Stdlib.Error bag -> Stdlib.Error (f bag)
