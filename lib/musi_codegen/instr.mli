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
  | LdC of int
  | LdCI4 of int32
  | LdCI4M1
  | LdCUnit
  | LdCStr of int
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

val to_opcode : t -> int
val show : t -> string
