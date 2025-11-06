(** Compilation pipeline orchestrating all compiler stages. *)

(** Check source text through pipeline.

    STAGES: Lexer -> Parser -> Resolver -> Checker *)
val check_source : string -> Diagnostic.bag

(** Compile source text to bytecode.

    STAGES: Lexer -> Parser -> Resolver -> Checker -> Codegen *)
val compile_source : string -> bytes

(** Read source file and check. *)
val check_file : string -> Diagnostic.bag

(** Compile source file to [<output>.msc] bytecode file. *)
val compile_file : string -> string -> unit

(** Format and write diagnostics with source context to stderr. *)
val print_diagnostics : Diagnostic.bag -> string -> unit
