(** Provides data structures for compiler diagnostics with source context and fix-it hints. *)

(** Distinguishes errors, warnings, and notes during reporting. *)
type severity = Error | Warning | Note

(** Encapsulates possible fix along with source span and replacement text. *)
type fixit = { span : Span.t; replacement : string }

(** Represents diagnostic data along with optional notes and fix-its. *)
type t = {
    severity : severity
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
  ; fixits : fixit list
}

(** Collects diagnostics while tracking error and warning counts. *)
type bag

(** Builds diagnostic value with provided severity, message, and span. *)
val make : severity -> string -> Span.t -> t

(** Builds error diagnostic for given source span. *)
val error : string -> Span.t -> t

(** Builds warning diagnostic for given source span. *)
val warning : string -> Span.t -> t

(** Builds note diagnostic for given source span. *)
val note : string -> Span.t -> t

(** Appends contextual note to existing diagnostic. *)
val with_note : t -> string -> Span.t -> t

(** Appends fix-it suggestion to existing diagnostic. *)
val with_fixit : t -> fixit -> t

(** Creates empty diagnostic bag. *)
val empty_bag : bag

(** Checks whether bag contains any diagnostics. *)
val is_empty : bag -> bool

(** Checks whether bag contains any errors. *)
val has_errors : bag -> bool

(** Adds diagnostic to bag. *)
val add : bag -> t -> bag

(** Lists diagnostics in order inserted. *)
val to_list : bag -> t list

(** Combines several bags into single bag. *)
val merge : bag list -> bag

(** Reports how many errors sit inside bag. *)
val error_count : bag -> int

(** Reports how many warnings sit inside bag. *)
val warning_count : bag -> int

(** Formats and writes diagnostic with source annotations to formatter. *)
val emit : Format.formatter -> t -> Source.t -> unit

(** Formats and writes every diagnostic inside bag to formatter. *)
val emit_all : Format.formatter -> bag -> Source.t -> unit
