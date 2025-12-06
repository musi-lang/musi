open Basic

type state = {
    source : string
  ; mutable pos : int
  ; len : int
  ; file_id : Interner.file_id
  ; interner : Interner.t
  ; mutable diags : Diagnostic.bag
}

val mk_state : string -> Interner.file_id -> Interner.t -> state
val peek : state -> char
val peek_n : state -> int -> char
val advance : state -> unit
val advance_n : state -> int -> unit

val tokenize :
     string
  -> Interner.file_id
  -> Interner.t
  -> (Token.t * Span.t) list * Diagnostic.bag
