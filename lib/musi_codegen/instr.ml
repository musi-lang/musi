open Musi_basic

type t =
  (* Base Instructions *)
  | Nop
  | Br of int
  | BrTrue of int
  | BrFalse of int
  | Beq of int
  | Bge of int
  | Bgt of int
  | Ble of int
  | Blt of int
  | Bne of int
  | Switch of int list
  | Leave of int
  | Ret
  | Throw
  | Rethrow
  | Try
  | Defer
  (* Stack Operations *)
  | Dup
  | Pop
  (* Load Constants *)
  | LdNull
  | LdCI4 of int32
  | LdCI8 of int64
  | LdCN1 of int
  | LdCN2 of int
  | LdCN4 of int
  | LdCN8 of int
  | LdCB4 of float
  | LdCB8 of float
  | LdCD4 of float
  | LdCD8 of float
  | LdStr of int
  (* Load/Store Variables *)
  | LdLoc of int
  | StLoc of int
  | LdLocA of int
  | LdArg of int
  | StArg of int
  | LdArgA of int
  (* Object Operations *)
  | NewObj of int
  | Call of Interner.name
  | CallInd
  | LdFld of int
  | StFld of int
  | LdFldA of int
  | LdSFld of int
  | StSFld of int
  | LdSFldA of int
  | NewArr of int
  | LdElem
  | StElem
  | LdElemA
  | LdLen
  (* Type Operations *)
  | LdTok of int
  | IsInst of int
  | AsInst of int
  | Box of int
  | UnboxAny of int
  | ConvI8
  | ConvI16
  | ConvI32
  | ConvI64
  | ConvI128
  | ConvN8
  | ConvN16
  | ConvN32
  | ConvN64
  | ConvN128
  | ConvB32
  | ConvB64
  | ConvB128
  | ConvD32
  | ConvD64
  | ConvD128
  (* Arithmetic Operations *)
  | Add
  | AddOvf
  | AddOvfUn
  | Sub
  | SubOvf
  | SubOvfUn
  | Mul
  | MulOvf
  | MulOvfUn
  | Div
  | DivUn
  | Rem
  | RemUn
  | Neg
  (* Logical/Bitwise Operations *)
  | And
  | Or
  | Xor
  | Not
  | Shl
  | Shr
  | ShrUn
  (* Comparison Operations *)
  | Ceq
  | Cgt
  | CgtUn
  | Clt
  | CltUn
  (* Memory Management - for future opt-in GC *)
  | Pin
  | Unpin
  (* Dynamic Operations *)
  | LdFldDyn of string
  | StFldDyn of string
  | LdFldADyn of string
  | CallDyn of string

let to_opcode = function
  (* Base Instructions - CIL compatible *)
  | Nop -> 0x00
  | Br _ -> 0x38
  | BrTrue _ -> 0x39
  | BrFalse _ -> 0x3A
  | Beq _ -> 0x3B
  | Bge _ -> 0x3C
  | Bgt _ -> 0x3D
  | Ble _ -> 0x3E
  | Blt _ -> 0x3F
  | Bne _ -> 0x40
  | Switch _ -> 0x45
  | Leave _ -> 0xC7
  | Ret -> 0x2A
  | Throw -> 0x7A
  | Rethrow -> 0x7B
  | Try -> 0xC8
  | Defer -> 0xC9
  (* Stack Operations *)
  | Dup -> 0x25
  | Pop -> 0x26
  (* Load Constants *)
  | LdNull -> 0x14
  | LdCI4 _ -> 0x20
  | LdCI8 _ -> 0x21
  | LdCN1 _ -> 0x22
  | LdCN2 _ -> 0x23
  | LdCN4 _ -> 0x24
  | LdCN8 _ -> 0x2F (* hypothetical value *)
  | LdCB4 _ -> 0x30 (* hypothetical value *)
  | LdCB8 _ -> 0x31 (* hypothetical value *)
  | LdCD4 _ -> 0x32 (* hypothetical value *)
  | LdCD8 _ -> 0x33 (* hypothetical value *)
  | LdStr _ -> 0x72
  (* Load/Store Variables *)
  | LdLoc _ -> 0x0E
  | StLoc _ -> 0x0F
  | LdLocA _ -> 0x10
  | LdArg _ -> 0x02
  | StArg _ -> 0x03
  | LdArgA _ -> 0x04
  (* Object Operations *)
  | NewObj _ -> 0x73
  | Call _ -> 0x28
  | CallInd -> 0x29
  | LdFld _ -> 0x7B
  | StFld _ -> 0x7D
  | LdFldA _ -> 0x7C
  | LdSFld _ -> 0x80
  | StSFld _ -> 0x81
  | LdSFldA _ -> 0x82
  | NewArr _ -> 0x8D
  | LdElem -> 0xA3
  | StElem -> 0xA4
  | LdElemA -> 0x8F
  | LdLen -> 0x8E
  (* Type Operations *)
  | LdTok _ -> 0xD1
  | IsInst _ -> 0x75
  | AsInst _ -> 0x74
  | Box _ -> 0x8C
  | UnboxAny _ -> 0xA5
  | ConvI8 -> 0x67
  | ConvI16 -> 0x68
  | ConvI32 -> 0x69
  | ConvI64 -> 0x6A
  | ConvI128 -> 0xD0
  | ConvN8 -> 0xD2
  | ConvN16 -> 0xD3
  | ConvN32 -> 0xD4
  | ConvN64 -> 0xD5
  | ConvN128 -> 0xD6
  | ConvB32 -> 0xB6
  | ConvB64 -> 0xB7
  | ConvB128 -> 0xD7
  | ConvD32 -> 0xD8
  | ConvD64 -> 0xD9
  | ConvD128 -> 0xDA
  (* Arithmetic Operations *)
  | Add -> 0x58
  | AddOvf -> 0xD6
  | AddOvfUn -> 0xD7
  | Sub -> 0x59
  | SubOvf -> 0xD8
  | SubOvfUn -> 0xD9
  | Mul -> 0x5A
  | MulOvf -> 0xDA
  | MulOvfUn -> 0xDB
  | Div -> 0x5B
  | DivUn -> 0x5C
  | Rem -> 0x5D
  | RemUn -> 0x5E
  | Neg -> 0x65
  (* Logical/Bitwise Operations *)
  | And -> 0x5F
  | Or -> 0x60
  | Xor -> 0x61
  | Not -> 0x66
  | Shl -> 0x62
  | Shr -> 0x63
  | ShrUn -> 0x64
  (* Comparison Operations *)
  | Ceq -> 0xFE
  | Cgt -> 0xC2
  | CgtUn -> 0xC3
  | Clt -> 0xC4
  | CltUn -> 0xC5
  (* Memory Management *)
  | Pin -> 0xDF
  | Unpin -> 0xE0
  (* Dynamic Operations *)
  | LdFldDyn _ -> 0xE1
  | StFldDyn _ -> 0xE2
  | LdFldADyn _ -> 0xE3
  | CallDyn _ -> 0xE4

let show = function
  (* Base Instructions *)
  | Nop -> "nop"
  | Br offset -> Printf.sprintf "br %d" offset
  | BrTrue offset -> Printf.sprintf "brtrue %d" offset
  | BrFalse offset -> Printf.sprintf "brfalse %d" offset
  | Beq offset -> Printf.sprintf "beq %d" offset
  | Bge offset -> Printf.sprintf "bge %d" offset
  | Bgt offset -> Printf.sprintf "bgt %d" offset
  | Ble offset -> Printf.sprintf "ble %d" offset
  | Blt offset -> Printf.sprintf "blt %d" offset
  | Bne offset -> Printf.sprintf "bne %d" offset
  | Switch offsets ->
    let offsets_str = String.concat ", " (List.map string_of_int offsets) in
    Printf.sprintf "switch [%s]" offsets_str
  | Leave offset -> Printf.sprintf "leave %d" offset
  | Ret -> "ret"
  | Throw -> "throw"
  | Rethrow -> "rethrow"
  | Try -> "try"
  | Defer -> "defer"
  (* Stack Operations *)
  | Dup -> "dup"
  | Pop -> "pop"
  (* Load Constants *)
  | LdNull -> "ldnull"
  | LdCI4 n -> Printf.sprintf "ldc.i4 %ld" n
  | LdCI8 n -> Printf.sprintf "ldc.i8 %Ld" n
  | LdCN1 n -> Printf.sprintf "ldc.n1 %d" n
  | LdCN2 n -> Printf.sprintf "ldc.n2 %d" n
  | LdCN4 n -> Printf.sprintf "ldc.n4 %d" n
  | LdCN8 n -> Printf.sprintf "ldc.n8 %d" n
  | LdCB4 f -> Printf.sprintf "ldc.b4 %g" f
  | LdCB8 f -> Printf.sprintf "ldc.b8 %g" f
  | LdCD4 f -> Printf.sprintf "ldc.d4 %g" f
  | LdCD8 f -> Printf.sprintf "ldc.d8 %g" f
  | LdStr idx -> Printf.sprintf "ldstr %d" idx
  (* Load/Store Variables *)
  | LdLoc idx -> Printf.sprintf "ldloc %d" idx
  | StLoc idx -> Printf.sprintf "stloc %d" idx
  | LdLocA idx -> Printf.sprintf "ldloca %d" idx
  | LdArg idx -> Printf.sprintf "ldarg %d" idx
  | StArg idx -> Printf.sprintf "starg %d" idx
  | LdArgA idx -> Printf.sprintf "ldarga %d" idx
  (* Object Operations *)
  | NewObj ctor -> Printf.sprintf "newobj %d" ctor
  | Call _method -> Printf.sprintf "call <method>"
  | CallInd -> "calli"
  | LdFld idx -> Printf.sprintf "ldfld %d" idx
  | StFld idx -> Printf.sprintf "stfld %d" idx
  | LdFldA idx -> Printf.sprintf "ldflda %d" idx
  | LdSFld idx -> Printf.sprintf "ldsfld %d" idx
  | StSFld idx -> Printf.sprintf "stsfld %d" idx
  | LdSFldA idx -> Printf.sprintf "ldsflda %d" idx
  | NewArr ty_id -> Printf.sprintf "newarr %d" ty_id
  | LdElem -> "ldelem"
  | StElem -> "stelem"
  | LdElemA -> "ldelema"
  | LdLen -> "ldlen"
  (* Type Operations *)
  | LdTok ty_id -> Printf.sprintf "ldtok %d" ty_id
  | IsInst ty_id -> Printf.sprintf "isinst %d" ty_id
  | AsInst ty_id -> Printf.sprintf "asinst %d" ty_id
  | Box ty_id -> Printf.sprintf "box %d" ty_id
  | UnboxAny ty_id -> Printf.sprintf "unbox.any %d" ty_id
  | ConvI8 -> "conv.i8"
  | ConvI16 -> "conv.i16"
  | ConvI32 -> "conv.i32"
  | ConvI64 -> "conv.i64"
  | ConvI128 -> "conv.i128"
  | ConvN8 -> "conv.n1"
  | ConvN16 -> "conv.n2"
  | ConvN32 -> "conv.n4"
  | ConvN64 -> "conv.n8"
  | ConvN128 -> "conv.n16"
  | ConvB32 -> "conv.b4"
  | ConvB64 -> "conv.b8"
  | ConvB128 -> "conv.b16"
  | ConvD32 -> "conv.d4"
  | ConvD64 -> "conv.d8"
  | ConvD128 -> "conv.d16"
  (* Arithmetic Operations *)
  | Add -> "add"
  | AddOvf -> "add.ovf"
  | AddOvfUn -> "add.ovf.un"
  | Sub -> "sub"
  | SubOvf -> "sub.ovf"
  | SubOvfUn -> "sub.ovf.un"
  | Mul -> "mul"
  | MulOvf -> "mul.ovf"
  | MulOvfUn -> "mul.ovf.un"
  | Div -> "div"
  | DivUn -> "div.un"
  | Rem -> "rem"
  | RemUn -> "rem.un"
  | Neg -> "neg"
  (* Logical/Bitwise Operations *)
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Not -> "not"
  | Shl -> "shl"
  | Shr -> "shr"
  | ShrUn -> "shr.un"
  (* Comparison Operations *)
  | Ceq -> "ceq"
  | Cgt -> "cgt"
  | CgtUn -> "cgt.un"
  | Clt -> "clt"
  | CltUn -> "clt.un"
  (* Memory Management *)
  | Pin -> "pin"
  | Unpin -> "unpin"
  (* Dynamic Operations *)
  | LdFldDyn name -> Printf.sprintf "ldfld.dyn \"%s\"" name
  | StFldDyn name -> Printf.sprintf "stfld.dyn \"%s\"" name
  | LdFldADyn name -> Printf.sprintf "ldflda.dyn \"%s\"" name
  | CallDyn name -> Printf.sprintf "call.dyn \"%s\"" name

let show_with_interner interner = function
  (* Base Instructions *)
  | Nop -> "nop"
  | Br offset -> Printf.sprintf "br %d" offset
  | BrTrue offset -> Printf.sprintf "brtrue %d" offset
  | BrFalse offset -> Printf.sprintf "brfalse %d" offset
  | Beq offset -> Printf.sprintf "beq %d" offset
  | Bge offset -> Printf.sprintf "bge %d" offset
  | Bgt offset -> Printf.sprintf "bgt %d" offset
  | Ble offset -> Printf.sprintf "ble %d" offset
  | Blt offset -> Printf.sprintf "blt %d" offset
  | Bne offset -> Printf.sprintf "bne %d" offset
  | Switch offsets ->
    let offsets_str = String.concat ", " (List.map string_of_int offsets) in
    Printf.sprintf "switch [%s]" offsets_str
  | Leave offset -> Printf.sprintf "leave %d" offset
  | Ret -> "ret"
  | Throw -> "throw"
  | Rethrow -> "rethrow"
  | Try -> "try"
  | Defer -> "defer"
  (* Stack Operations *)
  | Dup -> "dup"
  | Pop -> "pop"
  (* Load Constants *)
  | LdNull -> "ldnull"
  | LdCI4 n -> Printf.sprintf "ldc.i4 %ld" n
  | LdCI8 n -> Printf.sprintf "ldc.i8 %Ld" n
  | LdCN1 n -> Printf.sprintf "ldc.n1 %d" n
  | LdCN2 n -> Printf.sprintf "ldc.n2 %d" n
  | LdCN4 n -> Printf.sprintf "ldc.n4 %d" n
  | LdCN8 n -> Printf.sprintf "ldc.n8 %d" n
  | LdCB4 f -> Printf.sprintf "ldc.b4 %g" f
  | LdCB8 f -> Printf.sprintf "ldc.b8 %g" f
  | LdCD4 f -> Printf.sprintf "ldc.d4 %g" f
  | LdCD8 f -> Printf.sprintf "ldc.d8 %g" f
  | LdStr idx -> Printf.sprintf "ldstr %d" idx
  (* Load/Store Variables *)
  | LdLoc idx -> Printf.sprintf "ldloc %d" idx
  | StLoc idx -> Printf.sprintf "stloc %d" idx
  | LdLocA idx -> Printf.sprintf "ldloca %d" idx
  | LdArg idx -> Printf.sprintf "ldarg %d" idx
  | StArg idx -> Printf.sprintf "starg %d" idx
  | LdArgA idx -> Printf.sprintf "ldarga %d" idx
  (* Object Operations *)
  | NewObj ctor -> Printf.sprintf "newobj %d" ctor
  | Call name -> Printf.sprintf "call %s" (Interner.lookup interner name)
  | CallInd -> "callind"
  | LdFld idx -> Printf.sprintf "ldfld %d" idx
  | StFld idx -> Printf.sprintf "stfld %d" idx
  | LdFldA idx -> Printf.sprintf "ldflda %d" idx
  | LdSFld idx -> Printf.sprintf "ldsfld %d" idx
  | StSFld idx -> Printf.sprintf "stsfld %d" idx
  | LdSFldA idx -> Printf.sprintf "ldsflda %d" idx
  | NewArr ty_id -> Printf.sprintf "newarr %d" ty_id
  | LdElem -> "ldelem"
  | StElem -> "stelem"
  | LdElemA -> "ldelema"
  | LdLen -> "ldlen"
  (* Type Operations *)
  | LdTok ty_id -> Printf.sprintf "ldtok %d" ty_id
  | IsInst ty_id -> Printf.sprintf "isinst %d" ty_id
  | AsInst ty_id -> Printf.sprintf "asinst %d" ty_id
  | Box ty_id -> Printf.sprintf "box %d" ty_id
  | UnboxAny ty_id -> Printf.sprintf "unbox.any %d" ty_id
  | ConvI8 -> "conv.i8"
  | ConvI16 -> "conv.i16"
  | ConvI32 -> "conv.i32"
  | ConvI64 -> "conv.i64"
  | ConvI128 -> "conv.i128"
  | ConvN8 -> "conv.n1"
  | ConvN16 -> "conv.n2"
  | ConvN32 -> "conv.n4"
  | ConvN64 -> "conv.n8"
  | ConvN128 -> "conv.n16"
  | ConvB32 -> "conv.b4"
  | ConvB64 -> "conv.b8"
  | ConvB128 -> "conv.b16"
  | ConvD32 -> "conv.d4"
  | ConvD64 -> "conv.d8"
  | ConvD128 -> "conv.d16"
  (* Arithmetic Operations *)
  | Add -> "add"
  | AddOvf -> "add.ovf"
  | AddOvfUn -> "add.ovf.un"
  | Sub -> "sub"
  | SubOvf -> "sub.ovf"
  | SubOvfUn -> "sub.ovf.un"
  | Mul -> "mul"
  | MulOvf -> "mul.ovf"
  | MulOvfUn -> "mul.ovf.un"
  | Div -> "div"
  | DivUn -> "div.un"
  | Rem -> "rem"
  | RemUn -> "rem.un"
  | Neg -> "neg"
  (* Logical/Bitwise Operations *)
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Not -> "not"
  | Shl -> "shl"
  | Shr -> "shr"
  | ShrUn -> "shr.un"
  (* Comparison Operations *)
  | Ceq -> "ceq"
  | Cgt -> "cgt"
  | CgtUn -> "cgt.un"
  | Clt -> "clt"
  | CltUn -> "clt.un"
  (* Memory Management *)
  | Pin -> "pin"
  | Unpin -> "unpin"
  (* Dynamic Operations *)
  | LdFldDyn name -> Printf.sprintf "ldfld.dyn \"%s\"" name
  | StFldDyn name -> Printf.sprintf "stfld.dyn \"%s\"" name
  | LdFldADyn name -> Printf.sprintf "ldflda.dyn \"%s\"" name
  | CallDyn name -> Printf.sprintf "call.dyn \"%s\"" name
