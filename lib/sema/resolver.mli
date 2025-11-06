(** Runs name resolution pass. *)

(** Stores resolver state data. *)
type t

(** Builds resolver using provided interner. *)
val create : Interner.t -> t

(** Returns symbol table captured by resolver. *)
val symbols : t -> Symbol.t

(** Resolves names inside program and returns diagnostics. *)
val resolve : t -> Node.t list -> Diagnostic.bag
