(** Provides helpers that serialise compiled bytecode into [<output>.msil] file format. *)

(** Serialises provided array of procedure bytecode into [<output>.msil] payload. *)
val encode_program : Instr.t list array -> Metadata.module_desc -> bytes
