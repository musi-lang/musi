open Musi_basic

type t

val make : Interner.t -> t

val encode :
     t
  -> string list
  -> Emitter.const_kind list
  -> Emitter.proc_info list
  -> Instr.t list
  -> bytes
