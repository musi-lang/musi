open Musi_basic
open Musi_parse

type t =
  | TyUnit
  | TyBool
  | TyInt
  | TyNat
  | TyText
  | TyRune
  | TyTuple of t list
  | TyArray of t
  | TyRecord of (Interner.name * t) list
  | TyProc of t list * t
  | TyOptional of t
  | TyVar of var ref
  | TyError

and var = Unbound of int | Link of t

val fresh_var : unit -> t
val unify : t -> t -> unit
val occurs_check : int -> t -> bool
val repr : t -> t
val from_node : Node.ty -> t
val show : t -> string
