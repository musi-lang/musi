# SEAM Bytecode

Status: proposed

This spec defines SEAM BC/IL: the canonical stack-based bytecode transported by `.seam` and mirrored by `.seamil` text.

SEAM BC/IL is a portable VM substrate. It is not hard-wired to Musi source syntax. Musi, or any other frontend, targets SEAM by lowering source concepts to stack operations, metadata tables, descriptors, and runtime calls.

## Design Rules

Core rule: an opcode exists only when it is a primitive stack transition.

Each opcode must define:

- what it pops from the operand stack
- what it pushes to the operand stack
- which descriptor or immediate operand constrains it
- which verifier rule makes the transition valid

Source concepts do not name opcodes. There are no bytecode opcodes named `class`, `instance`, `given`, `shape`, `law`, `proof`, `answer`, `ask`, `handler`, `option`, `result`, `tuple`, `sum`, or `splice`.

## Rust Comparison

Rust 2024 is the host implementation language. SEAM bytecode is not Rust MIR, LLVM IR, or a Rust trait/object model.

Rust can inform implementation strategies such as enum layouts, call dispatch tables, and unsafe boundaries. Public SEAM remains stack bytecode plus stable descriptors.

## Reference Influences

SEAM naming uses existing VM instruction sets as spelling evidence, not as a rule to clone them:

- CIL: load/store roots, field roots, `call`, `callvirt`, `calli`, `ldftn`, `ldvirtftn`, `leave`
- JVM: virtual/interface/dynamic dispatch families
- WebAssembly: typed stack-machine arithmetic families and signed/unsigned suffix clarity
- LLVM: `resume` as a control-flow term

SEAM normalizes these into dotted text mnemonics.

## Text Mnemonics

`.seamil` uses dotted mnemonics. Binary `.seam` stores numeric opcodes; dots are text only.

Mnemonic grammar:

```text
segment      = lowercase-ascii-letter *(lowercase-ascii-letter / digit)
mnemonic     = segment *("." segment)
```

Rules:

- no aliases
- no leading, trailing, or repeated dots
- lowercase ASCII only
- dots separate stack action, target root, and qualifier
- `mod` is not a root because it conflicts with mathematical modulus
- `nat` is not a root because it conflicts with natural numbers

Canonical roots:

```text
ld      load/push value
st      store/pop into storage
new     allocate or construct value
init    initialize storage/value
cp      copy storage/value
dup     duplicate stack value/object
drop    consume stack value/object
br      branch
call    call
tail    tail-call qualifier
is      test/predicate
cast    checked cast
conv    conversion
box     box value
unbox   unbox value
hdl     handler/control frame
raise   raise resumable operation
resume  resume continuation
pin     pin managed object
unpin   release pin
arg     argument
loc     local
glob    global
fld     field
elem    indexed element
len     length
arr     array
obj     object/layout aggregate
fn      function/callable
virt    virtual receiver dispatch
iface   interface/shape/protocol dispatch
dyn     dynamic message dispatch
ind     indirect target/address from stack
ref     managed reference/view
ptr     native pointer
cont    continuation
imp     import
exp     export
mdl     module
ffi     foreign function interface / ABI edge
meta    metadata
attr    attribute
syn     syntax object, only in syntax-domain metadata
```

Meaning distinctions:

- `.ind` means the callable/address target comes from the operand stack.
- `.ref` means a managed reference/view value.
- `.ffi` means a foreign ABI boundary.
- `.dyn` means late-bound message/selector dispatch.

## Stack Types

Method signatures and block signatures are stack type lists.

```seamil
.method $pair (%x : Int, %y : Bool) -> [Int, Bool] locals [] {
entry stack []:
  ld.arg 0
  ld.arg 1
  ret
}
```

Rules:

- method result is a stack type list, not only one type
- block incoming stack is an exact stack type list
- each instruction transforms the current stack type list
- `ret` consumes exactly the method result stack list
- no implicit tuple allocation occurs for multi-result signatures
- a tuple/product value still uses `new.obj` when a storable value is needed

## Branch Stack Rule

Branches transfer the whole current stack.

Verifier rule:

1. `br.true` and `br.false` pop one `Bool`; `br.tbl` pops one integer index.
2. The remaining current stack must exactly match the target block `stack [...]` signature.
3. The target receives that whole stack.

There is no partial branch payload convention.

Example:

```seamil
left stack []:
  ld.loc 0
  br join

right stack []:
  ld.loc 1
  br join

join stack [Int]:
  ret
```

## Numeric Semantics

SEAM BC/IL is language-neutral. Source languages choose opcodes to express their own safety policy.

Locked arithmetic semantics:

- `add`, `sub`, and `mul` are wrapping/modulo for fixed-width integers
- `add`, `sub`, and `mul` use IEEE behavior for floating-point operands
- `add.ovf`, `sub.ovf`, and `mul.ovf` trap on integer overflow
- division, remainder, shifts, and ordered comparisons choose signed/unsigned interpretation explicitly with `.s` and `.u`
- `conv` performs the defined target conversion
- `conv.ovf` traps on target overflow or range loss

Musi source may still define checked arithmetic as its source-level default by emitting `.ovf` opcodes.

## Operand Kinds

Canonical operand kinds:

```text
none       no inline operand
u8/u16/u32 unsigned immediate
i32/i64    signed integer immediate
f32/f64    floating-point immediate
str        string table id
const      constant table id
type       type table id
sig        signature table id
method     method table id
member     member/slot descriptor id
field      field/layout descriptor id
global     global id
import     import id
export     export id
foreign    foreign ABI descriptor id
handler    resumable handler descriptor id
op         resumable operation descriptor id
block      block id
btbl       branch table id
token      typed metadata token id
```

## Opcode Table

Numeric opcode positions are canonical for this design. Gaps are reserved.

### `0x00..0x0F` Stack And Constants

|  Hex | Mnemonic   | Operand | Stack effect   | Meaning                                       |
| ---: | ---------- | ------- | -------------- | --------------------------------------------- |
| `00` | `nop`      | none    | `->`           | no-op                                         |
| `01` | `trap`     | str     | `-> !`         | unconditional trap                            |
| `02` | `pop`      | none    | `A ->`         | discard top value                             |
| `03` | `dup`      | none    | `A -> A, A`    | duplicate top value                           |
| `04` | `swap`     | none    | `A, B -> B, A` | swap top two values                           |
| `05` | `ld.unit`  | none    | `-> Unit`      | push unit value                               |
| `06` | `ld.true`  | none    | `-> Bool`      | push true                                     |
| `07` | `ld.false` | none    | `-> Bool`      | push false                                    |
| `08` | `ld.null`  | type    | `-> Ptr[T]`    | push typed null pointer/reference where legal |
| `09` | `ld.c`     | const   | `-> T`         | push constant table value                     |
| `0A` | `ld.c.i4`  | i32     | `-> Int32`     | push 32-bit integer immediate                 |
| `0B` | `ld.c.i8`  | i64     | `-> Int64`     | push 64-bit integer immediate                 |
| `0C` | `ld.c.f4`  | f32     | `-> Float32`   | push 32-bit float immediate                   |
| `0D` | `ld.c.f8`  | f64     | `-> Float64`   | push 64-bit float immediate                   |
| `0E` | `ld.str`   | str     | `-> String`    | push string table value                       |
| `0F` | reserved   |         |                |                                               |

### `0x10..0x1F` Storage And Fields

|       Hex | Mnemonic   | Operand | Stack effect    | Meaning                                          |
| --------: | ---------- | ------- | --------------- | ------------------------------------------------ |
|      `10` | `ld.arg`   | u16     | `-> T`          | load argument                                    |
|      `11` | `st.arg`   | u16     | `T ->`          | store argument when signature permits            |
|      `12` | `ld.loc`   | u16     | `-> T`          | load local                                       |
|      `13` | `st.loc`   | u16     | `T ->`          | store local                                      |
|      `14` | `ld.glob`  | global  | `-> T`          | load global                                      |
|      `15` | `st.glob`  | global  | `T ->`          | store global                                     |
|      `16` | `ld.fld`   | field   | `Obj -> T`      | load field by layout descriptor                  |
|      `17` | `st.fld`   | field   | `Obj, T ->`     | store field by layout descriptor                 |
|      `18` | `ld.fld.a` | field   | `Obj -> Ref[T]` | load field address/view                          |
|      `19` | `init.obj` | type    | `Ref[T] ->`     | initialize storage to default object/value state |
|      `1A` | `ld.dflt`  | type    | `-> T`          | push verifier-approved default value             |
| `1B`-`1F` | reserved   |         |                 |                                                  |

### `0x20..0x3F` Arithmetic, Bit Ops, Comparison

|  Hex | Mnemonic  | Operand | Stack effect      | Meaning                                |
| ---: | --------- | ------- | ----------------- | -------------------------------------- |
| `20` | `neg`     | none    | `N -> N`          | numeric negation                       |
| `21` | `add`     | none    | `N, N -> N`       | wrapping integer / IEEE float add      |
| `22` | `sub`     | none    | `N, N -> N`       | wrapping integer / IEEE float subtract |
| `23` | `mul`     | none    | `N, N -> N`       | wrapping integer / IEEE float multiply |
| `24` | `add.ovf` | none    | `Int, Int -> Int` | checked integer add                    |
| `25` | `sub.ovf` | none    | `Int, Int -> Int` | checked integer subtract               |
| `26` | `mul.ovf` | none    | `Int, Int -> Int` | checked integer multiply               |
| `27` | `div.s`   | none    | `Int, Int -> Int` | signed integer division                |
| `28` | `div.u`   | none    | `Int, Int -> Int` | unsigned integer division              |
| `29` | `rem.s`   | none    | `Int, Int -> Int` | signed integer remainder               |
| `2A` | `rem.u`   | none    | `Int, Int -> Int` | unsigned integer remainder             |
| `2B` | `and`     | none    | `A, A -> A`       | bitwise/boolean and                    |
| `2C` | `or`      | none    | `A, A -> A`       | bitwise/boolean or                     |
| `2D` | `xor`     | none    | `A, A -> A`       | bitwise/boolean xor                    |
| `2E` | `not`     | none    | `A -> A`          | bitwise/boolean not                    |
| `2F` | `shl`     | none    | `Int, Int -> Int` | shift left                             |
| `30` | `shr.s`   | none    | `Int, Int -> Int` | signed shift right                     |
| `31` | `shr.u`   | none    | `Int, Int -> Int` | unsigned shift right                   |
| `32` | `rotl`    | none    | `Int, Int -> Int` | rotate left                            |
| `33` | `rotr`    | none    | `Int, Int -> Int` | rotate right                           |
| `34` | `clz`     | none    | `Int -> Int`      | count leading zeros                    |
| `35` | `ctz`     | none    | `Int -> Int`      | count trailing zeros                   |
| `36` | `popcnt`  | none    | `Int -> Int`      | population count                       |
| `37` | `ceq`     | none    | `A, A -> Bool`    | equality compare                       |
| `38` | `cne`     | none    | `A, A -> Bool`    | inequality compare                     |
| `39` | `clt.s`   | none    | `A, A -> Bool`    | signed less-than                       |
| `3A` | `clt.u`   | none    | `A, A -> Bool`    | unsigned less-than                     |
| `3B` | `cgt.s`   | none    | `A, A -> Bool`    | signed greater-than                    |
| `3C` | `cgt.u`   | none    | `A, A -> Bool`    | unsigned greater-than                  |
| `3D` | `cle.s`   | none    | `A, A -> Bool`    | signed less/equal                      |
| `3E` | `cle.u`   | none    | `A, A -> Bool`    | unsigned less/equal                    |
| `3F` | reserved  |         |                   |                                        |

### `0x40..0x4F` Control Flow

|       Hex | Mnemonic   | Operand | Stack effect          | Meaning                                |
| --------: | ---------- | ------- | --------------------- | -------------------------------------- |
|      `40` | `cge.s`    | none    | `A, A -> Bool`        | signed greater/equal                   |
|      `41` | `cge.u`    | none    | `A, A -> Bool`        | unsigned greater/equal                 |
|      `42` | `br`       | block   | `S -> target.S`       | unconditional branch                   |
|      `43` | `br.true`  | block   | `S, Bool -> target.S` | branch if true                         |
|      `44` | `br.false` | block   | `S, Bool -> target.S` | branch if false                        |
|      `45` | `br.tbl`   | btbl    | `S, Int -> target.S`  | table branch                           |
|      `46` | `leave`    | block   | `S -> target.S`       | scoped branch with unwind              |
|      `47` | `ret`      | none    | `results ->`          | return method result stack             |
|      `48` | `throw`    | none    | `Obj -> !`            | throw runtime exception/failure object |
|      `49` | `rethrow`  | none    | `-> !`                | rethrow active exception/failure       |
|      `4A` | `unreach`  | none    | `-> !`                | verifier-known unreachable             |
| `4B`-`4F` | reserved   |         |                       |                                        |

### `0x50..0x6F` Calls And Function Values

|       Hex | Mnemonic          | Operand   | Stack effect                     | Meaning                             |
| --------: | ----------------- | --------- | -------------------------------- | ----------------------------------- |
|      `50` | `call`            | method    | `args -> results`                | direct managed call                 |
|      `51` | `call.ind`        | sig       | `Fn, args -> results`            | indirect first-class callable call  |
|      `52` | `call.virt`       | member    | `recv, args -> results`          | virtual receiver dispatch           |
|      `53` | `call.iface`      | member    | `recv, args -> results`          | interface/shape/protocol dispatch   |
|      `54` | `call.dyn`        | sig       | `recv, Message, args -> results` | late-bound message dispatch         |
|      `55` | `call.ffi`        | foreign   | `args -> results`                | foreign ABI call                    |
|      `56` | `tail.call`       | method    | `args -> results`                | direct tail call                    |
|      `57` | `tail.call.ind`   | sig       | `Fn, args -> results`            | indirect tail call                  |
|      `58` | `tail.call.virt`  | member    | `recv, args -> results`          | virtual tail call                   |
|      `59` | `tail.call.iface` | member    | `recv, args -> results`          | interface/shape tail call           |
|      `5A` | `tail.call.dyn`   | sig       | `recv, Message, args -> results` | dynamic tail call                   |
|      `5B` | `tail.call.ffi`   | foreign   | `args -> results`                | foreign ABI tail call               |
|      `5C` | `ld.fn`           | method    | `-> Fn`                          | load direct function value          |
|      `5D` | `new.fn`          | method,u8 | `captures -> Fn`                 | create closure/function value       |
|      `5E` | `ld.virt.fn`      | member    | `recv -> Fn`                     | load bound virtual member function  |
|      `5F` | `ld.iface.fn`     | member    | `recv -> Fn`                     | load bound interface/shape function |
|      `60` | `ld.dyn.fn`       | sig       | `recv, Message -> Fn`            | resolve dynamic message as callable |
|      `61` | `ld.ffi`          | foreign   | `-> FfiFn`                       | load foreign symbol handle          |
| `62`-`6F` | reserved          |           |                                  |                                     |

### `0x70..0x7F` Objects, Arrays, Elements

|       Hex | Mnemonic    | Operand  | Stack effect                | Meaning                          |
| --------: | ----------- | -------- | --------------------------- | -------------------------------- |
|      `70` | `new.obj`   | type,u16 | `fields -> Obj`             | construct layout object/value    |
|      `71` | `new.arr`   | type,u16 | `items -> Array[T]`         | construct array from stack items |
|      `72` | `arr.new`   | type     | `len -> Array[T]`           | allocate array by length         |
|      `73` | `ld.elem`   | type     | `Array[T], Int -> T`        | load element                     |
|      `74` | `st.elem`   | type     | `Array[T], Int, T ->`       | store element                    |
|      `75` | `ld.elem.a` | type     | `Array[T], Int -> Ref[T]`   | load element address/view        |
|      `76` | `ld.len`    | none     | `Array/String/Slice -> Int` | load length                      |
|      `77` | `slice`     | none     | `Seq, Int, Int -> Slice`    | create slice view                |
|      `78` | `cp.obj`    | type     | `Ref[T], Ref[T] ->`         | copy object/value storage        |
|      `79` | `dup.obj`   | type     | `T -> T, T`                 | duplicate copyable object/value  |
|      `7A` | `drop.obj`  | type     | `T ->`                      | drop object/value explicitly     |
| `7B`-`7F` | reserved    |          |                             |                                  |

### `0x80..0x8F` Types, Tokens, Conversions

|  Hex | Mnemonic     | Operand | Stack effect | Meaning                            |
| ---: | ------------ | ------- | ------------ | ---------------------------------- |
| `80` | `ld.type`    | type    | `-> Type`    | load type value/token              |
| `81` | `type.of`    | none    | `A -> Type`  | runtime type of value              |
| `82` | `is.inst`    | type    | `A -> Bool`  | runtime instance/refinement test   |
| `83` | `cast`       | type    | `A -> B`     | checked cast                       |
| `84` | `conv`       | type    | `A -> B`     | conversion to target type          |
| `85` | `conv.ovf`   | type    | `A -> B`     | checked conversion to target type  |
| `86` | `conv.s`     | type    | `A -> B`     | signed-source conversion           |
| `87` | `conv.u`     | type    | `A -> B`     | unsigned-source conversion         |
| `88` | `conv.ovf.s` | type    | `A -> B`     | checked signed-source conversion   |
| `89` | `conv.ovf.u` | type    | `A -> B`     | checked unsigned-source conversion |
| `8A` | `box`        | type    | `T -> Obj`   | box value                          |
| `8B` | `unbox`      | type    | `Obj -> T`   | unbox value                        |
| `8C` | `size.of`    | type    | `-> Int`     | ABI/layout size                    |
| `8D` | `align.of`   | type    | `-> Int`     | ABI/layout alignment               |
| `8E` | `ld.tok`     | token   | `-> Token`   | load typed metadata token          |
| `8F` | reserved     |         |              |                                    |

### `0x90..0x9F` References, Pointers, Pinning

|       Hex | Mnemonic   | Operand | Stack effect             | Meaning                                    |
| --------: | ---------- | ------- | ------------------------ | ------------------------------------------ |
|      `90` | `ld.ind`   | type    | `Ref[T]/Ptr[T] -> T`     | load through reference/pointer             |
|      `91` | `st.ind`   | type    | `Ref[T]/Ptr[T], T ->`    | store through reference/pointer            |
|      `92` | `ref.any`  | none    | `T -> Ref[T]`            | create verifier-approved managed reference |
|      `93` | `end.ref`  | none    | `Ref[T] ->`              | end non-owning reference lifetime          |
|      `94` | `pin`      | none    | `Obj -> Pin`             | pin managed object                         |
|      `95` | `unpin`    | none    | `Pin ->`                 | release pin                                |
|      `96` | `ld.addr`  | type    | `Pin -> Ptr[T]`          | load native address under active pin       |
|      `97` | `ptr.eq`   | none    | `Ptr[T], Ptr[T] -> Bool` | pointer equality                           |
|      `98` | `ptr.cast` | type    | `Ptr[A] -> Ptr[B]`       | pointer cast                               |
| `99`-`9F` | reserved   |         |                          |                                            |

There is no arbitrary pointer arithmetic opcode.

### `0xA0..0xAF` Resumable Control

|       Hex | Mnemonic    | Operand | Stack effect         | Meaning                               |
| --------: | ----------- | ------- | -------------------- | ------------------------------------- |
|      `A0` | `hdl.push`  | handler | `Hdl ->`             | push handler/control frame            |
|      `A1` | `hdl.pop`   | none    | `->`                 | pop handler/control frame             |
|      `A2` | `raise`     | op      | `args -> results`    | invoke resumable operation            |
|      `A3` | `resume`    | none    | `Cont, results -> !` | resume one-shot continuation          |
|      `A4` | `drop.cont` | none    | `Cont ->`            | consume continuation without resuming |
| `A5`-`AF` | reserved    |         |                      |                                       |

`raise` captures the continuation according to the operation descriptor. Handler clause methods receive the captured continuation as an ordinary parameter when the handler descriptor says they do.

### `0xB0..0xBF` Link And Module

|       Hex | Mnemonic   | Operand | Stack effect  | Meaning                          |
| --------: | ---------- | ------- | ------------- | -------------------------------- |
|      `B0` | `ld.imp`   | import  | `-> T`        | load linked import value         |
|      `B1` | `ld.exp`   | export  | `-> T`        | load current module export value |
|      `B2` | `mdl.load` | str     | `-> Module`   | dynamic module load              |
|      `B3` | `mdl.get`  | str     | `Module -> T` | dynamic module export lookup     |
| `B4`-`BF` | reserved   |         |               |                                  |

### `0xC0..0xFE` Reserved Domain Space

`0xC0..0xFE` are reserved for future standardized domain packs. They must not be used for frontend-private extensions in portable modules.

### `0xFF` Extended Opcode Escape

Extended opcode encoding:

```text
0xFF opcode:u16
```

Extended opcode mnemonics still use the same dotted mnemonic rules.

## Lowering Examples

### Tuple/Product

```seamil
ld.loc 0
ld.loc 1
new.obj $Tuple2 2
```

Access:

```seamil
ld.loc 2
ld.fld $Tuple2.0
```

### Sum Variant / Option / Result

```seamil
ld.loc payload
new.obj $Option.Some 1
```

Tag test:

```seamil
ld.loc opt
ld.fld $Option.tag
ld.c.i4 1
ceq
br.false none
```

### Shape / Interface Dispatch

Dictionary lowering:

```seamil
ld.loc dict
ld.fld $EqDict.equal
ld.loc a
ld.loc b
call.ind $EqEqualSig
```

VM-assisted interface/shape dispatch:

```seamil
ld.loc a
ld.loc b
call.iface $Eq.equal
```

### Dynamic Message Dispatch

```seamil
ld.loc recv
ld.c $msg.plus
ld.loc rhs
call.dyn $BinaryMessageSig
```

### Resumable Operation

```seamil
ld.loc hdl
hdl.push $Console.readLine
raise $Console.readLine
hdl.pop
```

Handler clause receives `Cont` as an ordinary argument when the handler descriptor needs resumption.

```seamil
ld.arg 0
ld.arg 1
resume
```

## See Also

- `specs/seam/format.md`
- `specs/seam/lowering.md`
- `specs/seam/domains.md`
