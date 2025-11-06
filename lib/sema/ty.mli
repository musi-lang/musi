(** Defines data structures used by bidirectional type checker. *)

(** Represents mutable type variables used during unification. *)
type ty_var = Unbound of int * int | Link of ty

(** Enumerates semantic types that flow through checker. *)
and ty = TyUnit | TyBool | TyInt | TyNat | TyVar of ty_var ref

(** Follows links until reaching canonical representative for given type variable. *)
val repr : ty -> ty

(** Checks whether given type variable appears within target type to prevent infinite types. *)
val occurs_check : int -> ty -> bool

(** Translates parsed node type into semantic representation. *)
val from_node_ty : Interner.t -> Node.ty -> ty

(** Renders type as readable string. *)
val show : ty -> string
