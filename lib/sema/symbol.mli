(** Holds symbol data used during name resolution. *)

(** Describes symbol categories tracked during resolution. *)
type symbol_kind =
  | SymVar of { mutable_ : bool }
  | SymProc of { param_count : int }

(** Stores metadata for single symbol tracked by resolver. *)
type symbol = {
    name : Interner.name
  ; kind : symbol_kind
  ; span : Span.t
  ; mutable ty : Ty.ty option
  ; mutable used : bool
}

(** Maintains symbol scopes with nested structure. *)
type t

(** Creates empty symbol table. *)
val create : unit -> t

(** Pushes new scope. *)
val push_scope : t -> unit

(** Pops current scope. *)
val pop_scope : t -> unit

(** Adds symbol to current scope. *)
val add : t -> symbol -> unit

(** Looks up symbol through scope chain. *)
val lookup : t -> Interner.name -> symbol option

(** Lists symbols currently visible in top scope. *)
val current_scope_symbols : t -> symbol list
