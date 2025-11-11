type t =
  (* Control *)
  | Nop
  | Br of int
  | BrTrue of int
  | BrFalse of int
  | Call of int
  | CallTail of int
  | Ret
  (* Stack *)
  | LdcI4 of int32
  | LdcI4M1
  | LdcUnit
  | LdcStr of int
  | LdLoc of int
  | StLoc of int
  | LdArg of int
  | Dup
  | Pop
  (* Arithmetic *)
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Neg
  (* Logical/Bitwise *)
  | And
  | Or
  | Xor
  | Not
  | Shl
  | Shr
  (* Comparison *)
  | CmpEq
  | CmpNe
  | CmpLt
  | CmpGt
  | CmpLe
  | CmpGe
  (* Object *)
  | NewObj of int
  | LdFld of int
  | StFld of int
  | LdElem
  | StElem
  | LdLen
  (* Type *)
  | IsInst of int
  | CastClass of int
  (* Memory *)
  | RefInc
  | RefDec

let to_opcode = function
  (* Control *)
  | Nop -> 0x00
  | Br _ -> 0x38
  | BrTrue _ -> 0x39
  | BrFalse _ -> 0x3A
  | Call _ -> 0x28
  | CallTail _ -> 0x29
  | Ret -> 0x2A
  (* Stack *)
  | LdcI4 _ -> 0x20
  | LdcI4M1 -> 0x15
  | LdcUnit -> 0x21
  | LdcStr _ -> 0x22
  | LdLoc _ -> 0x0E
  | StLoc _ -> 0x13
  | LdArg _ -> 0x09
  | Dup -> 0x25
  | Pop -> 0x26
  (* Arithmetic *)
  | Add -> 0x58
  | Sub -> 0x59
  | Mul -> 0x5A
  | Div -> 0x5B
  | Mod -> 0x5C
  | Neg -> 0x65
  (* Logical/Bitwise *)
  | And -> 0x5F
  | Or -> 0x60
  | Xor -> 0x61
  | Not -> 0x66
  | Shl -> 0x62
  | Shr -> 0x63
  (* Comparison *)
  | CmpEq -> 0xFE
  | CmpNe -> 0xFE
  | CmpLt -> 0xFE
  | CmpGt -> 0xFE
  | CmpLe -> 0xFE
  | CmpGe -> 0xFE
  (* Object *)
  | NewObj _ -> 0x73
  | LdFld _ -> 0x7B
  | StFld _ -> 0x7D
  | LdElem -> 0x8F
  | StElem -> 0x9C
  | LdLen -> 0x8E
  (* Type *)
  | IsInst _ -> 0x75
  | CastClass _ -> 0x74
  (* Memory *)
  | RefInc -> 0x40
  | RefDec -> 0x41

let show = function
  (* Control *)
  | Nop -> "nop"
  | Br offset -> Printf.sprintf "br %d" offset
  | BrTrue offset -> Printf.sprintf "br.true %d" offset
  | BrFalse offset -> Printf.sprintf "br.false %d" offset
  | Call proc_id -> Printf.sprintf "call %d" proc_id
  | CallTail proc_id -> Printf.sprintf "call.tail %d" proc_id
  | Ret -> "ret"
  (* Stack *)
  | LdcI4 n -> Printf.sprintf "ld.c.i4 %ld" n
  | LdcI4M1 -> "ld.c.i4.m1"
  | LdcUnit -> "ld.c.unit"
  | LdcStr idx -> Printf.sprintf "ld.c.str %d" idx
  | LdLoc idx -> Printf.sprintf "ld.loc %d" idx
  | StLoc idx -> Printf.sprintf "st.loc %d" idx
  | LdArg idx -> Printf.sprintf "ld.arg %d" idx
  | Dup -> "dup"
  | Pop -> "pop"
  (* Arithmetic *)
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Mod -> "mod"
  | Neg -> "neg"
  (* Logical/Bitwise *)
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Not -> "not"
  | Shl -> "shl"
  | Shr -> "shr"
  (* Comparison *)
  | CmpEq -> "cmp.eq"
  | CmpNe -> "cmp.ne"
  | CmpLt -> "cmp.lt"
  | CmpGt -> "cmp.gt"
  | CmpLe -> "cmp.le"
  | CmpGe -> "cmp.ge"
  (* Object *)
  | NewObj type_id -> Printf.sprintf "newobj %d" type_id
  | LdFld idx -> Printf.sprintf "ld.fld %d" idx
  | StFld idx -> Printf.sprintf "st.fld %d" idx
  | LdElem -> "ld.elem"
  | StElem -> "st.elem"
  | LdLen -> "ld.len"
  (* Type *)
  | IsInst type_id -> Printf.sprintf "isinst %d" type_id
  | CastClass type_id -> Printf.sprintf "castclass %d" type_id
  (* Memory *)
  | RefInc -> "ref.inc"
  | RefDec -> "ref.dec"
