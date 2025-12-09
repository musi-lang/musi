type severity = Error | Warning | Note

type t = {
    severity : severity
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
}

type bag = { diags : t list; errors : int; warnings : int }

val make : severity -> string -> Span.t -> t
val make_with_code : severity -> string -> Span.t -> t
val error : string -> Span.t -> t
val warning : string -> Span.t -> t
val note : string -> Span.t -> t
val add_note : string -> Span.t -> t -> t
val empty_bag : bag
val add : bag -> t -> bag
val add_list : bag -> t list -> bag
val merge : bag -> bag -> bag
val get_errors : bag -> t list
val get_warnings : bag -> t list
val get_all : bag -> t list
val count_errors : bag -> int
val count_warnings : bag -> int
val is_empty : bag -> bool
