open Musi_basic

type t

val make : Span.file_id -> string -> Interner.t -> t
val lex_all : t -> Token.t list * Diagnostic.bag
