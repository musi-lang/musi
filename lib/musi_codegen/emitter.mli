open Musi_basic
open Musi_parse

type const_kind =
  | ConstInt of int64
  | ConstBin of float
  | ConstStr of Interner.name
  | ConstBool of bool
  | ConstUnit

type t

val make : unit -> t
val emit : t -> Node.stmt list -> Instr.t list
val const_pool : t -> const_kind list
