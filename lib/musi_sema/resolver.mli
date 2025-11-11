open Musi_basic
open Musi_parse

val resolve : Node.stmt list -> Interner.t -> Diagnostic.bag ref -> Symbol.table

val resolve_with_table :
     Symbol.table
  -> Node.stmt list
  -> Interner.t
  -> Diagnostic.bag ref
  -> Symbol.table
