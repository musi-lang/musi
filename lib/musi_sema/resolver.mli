open Musi_basic
open Musi_parse

val resolve :
     ?base_path:string
  -> Node.stmt list
  -> Interner.t
  -> Diagnostic.bag ref
  -> Symbol.table
