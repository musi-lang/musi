(** Provides functions that turn typed AST nodes into bytecode chunks. *)

(** Holds identifier plus frame sizes gathered during first emission pass. *)
type proc_info = { id : int; param_count : int; local_count : int }

(** Maps interned procedure names to their collected metadata. *)
type proc_table = (Interner.name, proc_info) Hashtbl.t

(** Assigns numeric identifiers to named procedures while reserving identifier 0 for module entry point. *)
val collect_procs : Interner.t -> Node.t list -> proc_table

(** Produces bytecode sequences for every procedure with index 0 reserved for top-level code. Returns procedures and module metadata. *)
val emit_program :
     Interner.t
  -> proc_table
  -> Diagnostic.bag ref
  -> Node.t list
  -> Instr.t list array * Metadata.module_desc
