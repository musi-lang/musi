open Musi_basic

type t = {
    name : Interner.name
  ; ty : Types.t ref
  ; is_mutable : bool
  ; span : Span.t
}

type scope
type table

val empty_table : unit -> table
val enter_scope : table -> table
val exit_scope : table -> table
val bind : table -> Interner.name -> t -> table
val lookup : table -> Interner.name -> t option
val lookup_current : table -> Interner.name -> t option
