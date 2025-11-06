(** Represents mutable state used while parsing modules. *)
type t

(** Constructs parser from provided token stream and interner. *)
val make : Token.t list -> Interner.t -> t

(** Parses complete module and returns AST plus diagnostics. *)
val parse : t -> Node.t list * Diagnostic.bag
