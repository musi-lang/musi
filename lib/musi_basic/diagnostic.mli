type severity = Error | Warning | Note

type t = {
    severity : severity
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
}

type bag

val make : severity -> string -> Span.t -> t
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
val emit : Format.formatter -> t -> Source.t -> unit
val emit_all : Format.formatter -> bag -> Source.t -> unit
