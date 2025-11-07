type header = {
    magic : string
  ; version : int32
  ; bc_offset : int32
  ; bc_size : int32
  ; metadata_offset : int32
  ; metadata_size : int32
  ; reserved : int64
}

type constant = ConstI32 of int32 | ConstI64 of int64 | ConstText of string

type proc_desc = {
    name : string option
  ; param_count : int
  ; local_count : int
  ; max_stack : int
  ; bytecode_offset : int
  ; bytecode_length : int
  ; is_extern : bool
}

type proc_info = { param_count : int }

type module_desc = {
    module_name : string option
  ; exports : (string * int) list
  ; link_keys : (int * string) list
  ; const_pool : constant list
  ; proc_infos : proc_info list
  ; imports : (string * string list) list
}

type optional_sections = {
    export_table : (string * int) list
  ; link_table : (int * string) list
}
