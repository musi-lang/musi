(** Provides helpers that serialise compiled bytecode into [<output>.msc] file format. *)

(** Serialises provided array of procedure bytecode into [<output>.msc] payload. *)
val encode_program : Instr.t list array -> Metadata.module_desc -> bytes
