(** Lists constructors for every bytecode instruction emitted by compiler. *)

(** Describes discriminated union representing each bytecode instruction. *)
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
  | LdcStr of int
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

(** Returns opcode byte linked with provided instruction. *)
val to_opcode : t -> int

(** Renders instruction as readable string. *)
val show : t -> string
