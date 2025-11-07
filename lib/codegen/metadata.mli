(** Describes metadata stored inside bytecode output. *)

(** Explains header block written near start for bytecode file. *)
type header = {
    magic : string
  ; version : int32
  ; bc_offset : int32
  ; bc_size : int32
  ; metadata_offset : int32
  ; metadata_size : int32
  ; reserved : int64
}

(** Describes single entry stored inside constant pool segment. *)
type constant = ConstI32 of int32 | ConstI64 of int64 | ConstText of string

(** Captures data needed to locate and run compiled procedure. *)
type proc_desc = {
    name : string option
  ; param_count : int
  ; local_count : int
  ; max_stack : int
  ; bytecode_offset : int
  ; bytecode_length : int
  ; is_extern : bool
}

(** Stores parameter count for procedure. *)
type proc_info = { param_count : int }

(** Tracks module name along with exported entries and link keys. *)
type module_desc = {
    module_name : string option
  ; exports : (string * int) list
  ; link_keys : (int * string) list
  ; const_pool : constant list
  ; proc_infos : proc_info list
  ; imports : (string * string list) list
}

(** Optional sections appended after bytecode payload. *)
type optional_sections = {
    export_table : (string * int) list
  ; link_table : (int * string) list
}
