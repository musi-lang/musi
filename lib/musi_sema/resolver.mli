open Musi_basic
open Musi_parse

val resolve : Node.stmt list -> Interner.t -> Diagnostic.bag ref -> Symbol.table
