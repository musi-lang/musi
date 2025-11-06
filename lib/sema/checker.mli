(** Runs bidirectional type checker. *)

(** Stores checker state data. *)
type t

(** Builds checker with interner and resolved symbols. *)
val create : Interner.t -> Symbol.t -> t

(** Checks types for program and returns diagnostics. *)
val check : t -> Node.t list -> Diagnostic.bag
