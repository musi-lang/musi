type t =
  | Nop
  | LdcI4 of int32
  | LdcI4M1
  | LdcI4_0
  | LdcI4_1
  | LdcI4_2
  | LdcI4_3
  | LdcI4_4
  | LdcI4_5
  | LdcI4_6
  | LdcI4_7
  | LdcI4_8
  | LdcUnit
  | LdLoc of int
  | StLoc of int
  | LdArg of int
  | Dup
  | Pop
  | Add
  | Sub
  | Mul
  | Div
  | Neg
  | CmpEq
  | CmpNe
  | CmpLt
  | CmpGt
  | CmpLe
  | CmpGe
  | Br of int
  | BrTrue of int
  | BrFalse of int
  | Call of int
  | Ret

let to_opcode = function
  | Nop -> 0x00
  | LdcI4 _ -> 0x20
  | LdcI4M1 -> 0x15
  | LdcI4_0 -> 0x16
  | LdcI4_1 -> 0x17
  | LdcI4_2 -> 0x18
  | LdcI4_3 -> 0x19
  | LdcI4_4 -> 0x1A
  | LdcI4_5 -> 0x1B
  | LdcI4_6 -> 0x1C
  | LdcI4_7 -> 0x1D
  | LdcI4_8 -> 0x1E
  | LdcUnit -> 0x21
  | LdLoc _ -> 0x0E
  | StLoc _ -> 0x13
  | LdArg _ -> 0x09
  | Dup -> 0x25
  | Pop -> 0x26
  | Add -> 0x58
  | Sub -> 0x59
  | Mul -> 0x5A
  | Div -> 0x5B
  | Neg -> 0x65
  | CmpEq -> 0xFE
  | CmpNe -> 0xFE
  | CmpLt -> 0xFE
  | CmpGt -> 0xFE
  | CmpLe -> 0xFE
  | CmpGe -> 0xFE
  | Br _ -> 0x38
  | BrTrue _ -> 0x39
  | BrFalse _ -> 0x3A
  | Call _ -> 0x28
  | Ret -> 0x2A

let show = function
  | Nop -> "nop"
  | LdcI4 n -> Printf.sprintf "ld.c.i4 %ld" n
  | LdcI4M1 -> "ld.c.i4.m1"
  | LdcI4_0 -> "ld.c.i4.0"
  | LdcI4_1 -> "ld.c.i4.1"
  | LdcI4_2 -> "ld.c.i4.2"
  | LdcI4_3 -> "ld.c.i4.3"
  | LdcI4_4 -> "ld.c.i4.4"
  | LdcI4_5 -> "ld.c.i4.5"
  | LdcI4_6 -> "ld.c.i4.6"
  | LdcI4_7 -> "ld.c.i4.7"
  | LdcI4_8 -> "ld.c.i4.8"
  | LdcUnit -> "ld.c.unit"
  | LdLoc idx -> Printf.sprintf "ld.loc %d" idx
  | StLoc idx -> Printf.sprintf "st.loc %d" idx
  | LdArg idx -> Printf.sprintf "ld.arg %d" idx
  | Dup -> "dup"
  | Pop -> "pop"
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Neg -> "neg"
  | CmpEq -> "cmp.eq"
  | CmpNe -> "cmp.ne"
  | CmpLt -> "cmp.lt"
  | CmpGt -> "cmp.gt"
  | CmpLe -> "cmp.le"
  | CmpGe -> "cmp.ge"
  | Br offset -> Printf.sprintf "br %d" offset
  | BrTrue offset -> Printf.sprintf "br.true %d" offset
  | BrFalse offset -> Printf.sprintf "br.false %d" offset
  | Call proc_id -> Printf.sprintf "call %d" proc_id
  | Ret -> "ret"
