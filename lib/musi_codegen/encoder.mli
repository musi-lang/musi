open Musi_basic

type t

val make : Interner.t -> t
val encode : t -> Emitter.proc_info list -> Instr.t list -> bytes
