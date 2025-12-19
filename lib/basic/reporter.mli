(** Diagnostic severity level *)
type level = Error | Warning | Note

(** Diagnostic message with location *)
type t = {
    level : level
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
}

(** Collection of diagnostics *)
type bag = { diags : t list; errors : int; warnings : int }

(** Result type carrying diagnostic bag *)
type 'a result = ('a, bag) Stdlib.result

(** Create diagnostic *)
val make : level -> string -> Span.t -> t

(** Create error diagnostic *)
val error : string -> Span.t -> t

(** Create warning diagnostic *)
val warning : string -> Span.t -> t

(** Create note diagnostic *)
val note : string -> Span.t -> t

(** Add note to diagnostic *)
val with_note : t -> string -> Span.t -> t

(** Empty diagnostic bag *)
val empty_bag : bag

(** Check if bag is empty *)
val is_empty : bag -> bool

(** Check if bag contains errors *)
val has_errors : bag -> bool

(** Add diagnostic to bag *)
val add : bag -> t -> bag

(** Convert bag to list of diagnostics *)
val to_list : bag -> t list

(** Merge multiple bags *)
val merge : bag list -> bag

(** Get error count *)
val error_count : bag -> int

(** Get warning count *)
val warning_count : bag -> int

(** Emit single diagnostic to formatter *)
val emit : Format.formatter -> t -> (int * Source.t) list -> unit

(** Emit all diagnostics in bag *)
val emit_all : Format.formatter -> bag -> (int * Source.t) list -> unit

(** Create successful result *)
val try_ok : 'a -> 'a result

(** Create error result from bag *)
val try_error_bag : bag -> 'a result

(** Create error result from single diagnostic *)
val try_error_diag : t -> 'a result

(** Create error result from message and span *)
val try_error_info : string -> Span.t -> 'a result

(** Create warning result with value *)
val try_warning_diag : t -> 'a result

(** Create warning result from message and span *)
val try_warning_info : string -> Span.t -> 'a result

(** Bind result monad *)
val try_bind : ('a -> 'b result) -> 'a result -> 'b result

(** Map result success value *)
val try_map : ('a -> 'b) -> 'a result -> 'b result

(** Map result error bag *)
val try_map_error : (bag -> bag) -> 'a result -> 'a result
