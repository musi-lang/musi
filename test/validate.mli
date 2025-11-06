(** Fails when trimmed bytecode snippets differ. *)
val check_bytecode : expected:string -> actual:string -> unit

(** Fails when diagnostics bag reports errors. *)
val check_no_errors : diags:Diagnostic.bag -> unit

(** Fails when diagnostics bag holds no errors. *)
val check_has_errors : diags:Diagnostic.bag -> unit
