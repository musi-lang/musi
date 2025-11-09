open Musi_basic
open Musi_lex

type t

val make : Token.t list -> Interner.t -> t
val parse : t -> Node.stmt list * Diagnostic.bag
