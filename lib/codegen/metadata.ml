type header = {
    magic : string
  ; version : int32
  ; bc_offset : int32
  ; bc_size : int32
  ; export_offset : int32
  ; export_count : int32
  ; link_offset : int32
  ; link_count : int32
}

type constant = ConstI32 of int32 | ConstI64 of int64 | ConstText of string

type proc_desc = {
    name : string option
  ; param_count : int
  ; local_count : int
  ; max_stack : int
  ; bytecode_offset : int
  ; bytecode_length : int
}

type module_desc = {
    module_name : string option
  ; exports : (string * int) list
  ; link_keys : (int * string) list
}

type optional_sections = {
    export_table : (string * int) list
  ; link_table : (int * string) list
}
