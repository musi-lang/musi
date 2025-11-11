open Musi_basic

type t

val make : Interner.t -> t
val encode : t -> Instr.t list -> bytes
