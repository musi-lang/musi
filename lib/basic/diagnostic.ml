module type S = sig
  type severity = Error | Warning | Note
  type category = Lex | Parse | Sema | Codegen | Runtime

  type error_code =
    (* E0000 .. E0999 *)
    | Lex of string
    (* E1000 .. E1999 *)
    | Parse of string
    (* E2000 .. E2999 *)
    | Sema of string
    (* E3000 .. E3999 *)
    | Codegen of string
    (* E4000 .. E4999 *)
    | Runtime of string

  type t = {
      severity : severity
    ; code : error_code option
    ; message : string
    ; span : Span.t
    ; notes : (string * Span.t) list
  }

  type bag = { diags : t list; errors : int; warnings : int }

  val make : severity -> string -> Span.t -> t
  val make_with_code : severity -> error_code -> string -> Span.t -> t
  val error : string -> Span.t -> t
  val warning : string -> Span.t -> t
  val note : string -> Span.t -> t
  val error_with_code : error_code -> string -> Span.t -> t
  val warning_with_code : error_code -> string -> Span.t -> t
  val note_with_code : error_code -> string -> Span.t -> t
  val with_note : t -> string -> Span.t -> t
  val empty_bag : bag
  val is_empty : bag -> bool
  val has_errors : bag -> bool
  val add : bag -> t -> bag
  val to_list : bag -> t list
  val merge : bag list -> bag
  val error_count : bag -> int
  val warning_count : bag -> int
  val emit : Format.formatter -> t -> Source.t -> unit
  val emit_all : Format.formatter -> bag -> Source.t -> unit
  val has_message_containing : bag -> string -> bool
end

module Make () : S = struct
  type severity = Error | Warning | Note
  type category = Lex | Parse | Sema | Codegen | Runtime

  type error_code =
    | Lex of string
    | Parse of string
    | Sema of string
    | Codegen of string
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
  let with_note t message span = { t with notes = (message, span) :: t.notes }
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

  let severity_header ppf = function
    | Error -> styled `Bold ppf "error:"
    | Warning -> styled `Bold ppf "warning:"
    | Note -> styled `Bold ppf "note:"

  let severity_color = function
    | Error -> `Red
    | Warning -> `Yellow
    | Note -> `Cyan

  let emit_location ppf file line col =
    Format.fprintf ppf "%a:%d:%d:" (styled `Bold) (Source.path file) line col

  let emit_error_code ppf = function
    | None -> ()
    | Some code ->
      let code_str =
        match code with Lex s | Parse s | Sema s | Codegen s | Runtime s -> s
      in
      Format.fprintf ppf " [%s]" code_str

  let emit_source_line ppf line_num text =
    Format.fprintf ppf " %a | %s@." (styled `Bold) (string_of_int line_num) text

  let emit_caret ppf line_num col len color =
    let padding = String.make (String.length (string_of_int line_num)) ' ' in
    let spaces = String.make (col - 1) ' ' in
    let carets = String.make len '^' in
    Format.fprintf ppf " %s | %s%a@." padding spaces (styled (`Fg color)) carets

  let emit_note ppf files msg span =
    match Source.get_file files (Span.file span) with
    | None ->
      severity_header ppf Note;
      Format.fprintf ppf " %s@." msg
    | Some f ->
      let l, c = Source.line_col f (Span.start span) in
      emit_location ppf f l c;
      Format.fprintf ppf " ";
      severity_header ppf Note;
      Format.fprintf ppf " %s@." msg

  let emit ppf diag files =
    match Source.get_file files (Span.file diag.span) with
    | None ->
      severity_header ppf diag.severity;
      emit_error_code ppf diag.code;
      Format.fprintf ppf " %s@." diag.message
    | Some file ->
      let line, col = Source.line_col file (Span.start diag.span) in
      emit_location ppf file line col;
      Format.fprintf ppf " ";
      severity_header ppf diag.severity;
      emit_error_code ppf diag.code;
      Format.fprintf ppf " %s@." diag.message;
      (match Source.line_text file line with
      | None -> ()
      | Some text ->
        emit_source_line ppf line text;
        let end_line, end_col = Source.line_col file (Span.end_ diag.span) in
        let len =
          if line = end_line then max 1 (end_col - col)
          else max 1 (String.length text - col + 1)
        in
        emit_caret ppf line col len (severity_color diag.severity));
      List.iter
        (fun (msg, span) -> emit_note ppf files msg span)
        (List.rev diag.notes)

  let emit_all ppf bag files =
    List.iter (fun d -> emit ppf d files) (to_list bag)

  let has_message_containing bag substring =
    let diag_list = to_list bag in
    let contains_substring str sub =
      let len_str = String.length str in
      let len_sub = String.length sub in
      let rec check i =
        if i + len_sub > len_str then false
        else if String.sub str i len_sub = sub then true
        else check (i + 1)
      in
      if len_sub = 0 then true else check 0
    in
    List.exists
      (fun diag -> contains_substring diag.message substring)
      diag_list
end

include Make ()
