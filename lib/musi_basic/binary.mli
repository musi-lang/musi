val read_u16_le : bytes -> int -> int
val read_u32_le : bytes -> int -> int32
val read_i32_le : bytes -> int -> int32
val read_i64_le : bytes -> int -> int64
val write_u16_le : Buffer.t -> int -> unit
val write_u32_le : Buffer.t -> int32 -> unit
val write_i32_le : Buffer.t -> int32 -> unit
val write_i64_le : Buffer.t -> int64 -> unit
