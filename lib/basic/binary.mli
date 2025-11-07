(** Provides helpers for reading and writing little-endian integers. *)

(** Reads 16-bit unsigned integer from buffer using little-endian order. *)
val read_u16_le : bytes -> int -> int

(** Reads 32-bit unsigned integer from buffer using little-endian order. *)
val read_u32_le : bytes -> int -> int32

(** Reads 32-bit signed integer from buffer using little-endian order. *)
val read_i32_le : bytes -> int -> int32

(** Writes 16-bit unsigned integer into buffer using little-endian order. *)
val write_u16_le : Buffer.t -> int -> unit

(** Writes 32-bit unsigned integer into buffer using little-endian order. *)
val write_u32_le : Buffer.t -> int32 -> unit

(** Writes 32-bit signed integer into buffer using little-endian order. *)
val write_i32_le : Buffer.t -> int32 -> unit

(** Writes 64-bit signed integer into buffer using little-endian order. *)
val write_i64_le : Buffer.t -> int64 -> unit
