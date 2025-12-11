type level = Error | Warning | Note

type t = {
    level : level
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
}

type bag = { diags : t list; errors : int; warnings : int }
type 'a result = ('a, bag) Stdlib.result

val make : level -> string -> Span.t -> t
val error : string -> Span.t -> t
val warning : string -> Span.t -> t
val note : string -> Span.t -> t
val with_note : t -> string -> Span.t -> t
val empty_bag : bag
val is_empty : bag -> bool
val has_errors : bag -> bool
val add : bag -> t -> bag
val to_list : bag -> t list
val merge : bag list -> bag
val error_count : bag -> int
val warning_count : bag -> int
val emit : Format.formatter -> t -> (int * Source.t) list -> unit
val emit_all : Format.formatter -> bag -> (int * Source.t) list -> unit
val try_ok : 'a -> 'a result
val try_error_bag : bag -> 'a result
val try_error_diag : t -> 'a result
val try_error_info : string -> Span.t -> 'a result
val try_warning_diag : t -> 'a result
val try_warning_info : string -> Span.t -> 'a result
val try_bind : ('a -> 'b result) -> 'a result -> 'b result
val try_map : ('a -> 'b) -> 'a result -> 'b result
val try_map_error : (bag -> bag) -> 'a result -> 'a result
