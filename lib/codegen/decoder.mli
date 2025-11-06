(** Provides helpers that decode stored bytecode modules. *)

(** Collects header, bytecode, exports, and link keys after decoding. *)
type decoded_program = {
    header : Metadata.header
  ; bytecode : bytes
  ; exports : (string * int) list
  ; links : (int * string) list
}

(** Parses raw bytes into decoded program or error message. *)
val decode_program : bytes -> (decoded_program, string) result
