open Musi_basic
open Musi_parse

val check :
  Node.stmt list -> Symbol.table -> Interner.t -> Diagnostic.bag ref -> unit

val check_all : Node.stmt list -> Interner.t -> Diagnostic.bag ref -> unit
