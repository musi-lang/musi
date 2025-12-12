open Basic

type t

val create : Source.t -> int -> t
val with_interner : Interner.t -> Source.t -> int -> t
val try_tokenize : t -> (Token.t * Span.t) list Reporter.result
val try_next_token : t -> (Token.t * Span.t) Reporter.result
val curr_pos : t -> int
val source : t -> Source.t
val file_id : t -> int
val has_errors : t -> bool
val error_bag : t -> Reporter.bag
val emit_errors : Format.formatter -> t -> unit
val token_stream_opt : t -> (Token.t * Span.t) Seq.t option
