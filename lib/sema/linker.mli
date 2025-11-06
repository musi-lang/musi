(** Handles module linking and dependency resolution. *)

(** Identifies module using stable string key. *)
type module_id = string

(** Stores parsed module data plus import and export sets. *)
type module_info = {
    id : module_id
  ; path : string
  ; ast : Node.t list
  ; imports : module_id list
  ; exports : Interner.name list
}

(** Keeps internal linker state across passes. *)
type t

(** Builds linker state storage. *)
val create : Interner.t -> t

(** Builds import graph from entry file and returns modules with diagnostics. *)
val build_import_graph : t -> string -> module_info list * Diagnostic.bag

(** Performs topological sort with cycle detection and returns sorted modules with diagnostics. *)
val topological_sort :
  t -> module_info list -> module_info list * Diagnostic.bag
