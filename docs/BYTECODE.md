# Musi Bytecode Specification

## Abstract

MSIL (Musi Stack-based Intermediate Language) is a general-purpose bytecode format designed for efficient execution. Inspired by CIL and Java bytecode but optimized for performance and hardware proximity. The instruction set focuses on what CPUs care about: control flow, arithmetic, memory access, and type-aware operations without language-specific constructs.

**Design Principles**:

- **Hardware-inspired**: General-purpose opcodes similar to CPU instruction sets
- **Type-tagged values**: Operands carry type tags for safety without runtime checks
- **Stack-based**: Simple, fast evaluation model with minimal state
- **No language assumptions**: No built-in support for lists, classes, ranges, etc. — these are library constructs

## 1. File Format (.mso)

### 1.1 Module Structure

Compiled Musi modules use the `.mso` (Musi Stack Object) format:

```ebnf
mso_file ::= header const_pool symbol_table type_table function_table code_section attr_table
```

```text
+-------------------+
|      Header       |  magic, version, flags, section sizes
+-------------------+
|   Constant Pool   |  literals (strings/numbers)
+-------------------+
|   Symbol  Table   |  names (func/type/field)
+-------------------+
|    Type Table     |  type descriptors
+-------------------+
|  Function Table   |  signatures, locals, code offsets
+-------------------+
|   Code  Section   |  bytecode instructions
+-------------------+
|  Attribute Table  |  optional metadata (debug, docs)
+-------------------+
```

Notes:

- Tables are dense and indexed; no per-entry attribute lists unless enabled.
- `attr_table` is optional and may be omitted in release builds.

### 1.2 Header Format

```hex
Offset  Size  Field
------  ----  -----
0x00    4     magic: 0x4D555249 (ASCII "MUSI")
0x04    4     version: u32 (format version)
0x08    4     flags: u32 (bit flags, reserved)
0x0C    4     const_pool_len: u32 (entry count)
0x10    4     symbol_table_len: u32 (entry count)
0x14    4     type_table_len: u32 (entry count)
0x18    4     function_table_len: u32 (entry count)
0x1C    4     code_section_len: u32 (byte length)
0x20    4     entry_point: u32 (symbol table index)
0x24    12    reserved (zero)
```

### 1.3 Type Notation

Instruction operands use the following type prefixes:

| Prefix | Type                    | Size   | Encoding        |
|--------|-------------------------|--------|-----------------|
| `i8`   | signed 8-bit integer    | 1 byte | two's complement |
| `i16`  | signed 16-bit integer   | 2 bytes| little-endian   |
| `i32`  | signed 32-bit integer   | 4 bytes| little-endian   |
| `i64`  | signed 64-bit integer   | 8 bytes| little-endian   |
| `n8`   | unsigned 8-bit integer  | 1 byte | binary          |
| `n16`  | unsigned 16-bit integer | 2 bytes| little-endian   |
| `n32`  | unsigned 32-bit integer | 4 bytes| little-endian   |
| `n64`  | unsigned 64-bit integer | 8 bytes| little-endian   |
| `r32`  | IEEE 754 single float   | 4 bytes| little-endian   |
| `r64`  | IEEE 754 double float   | 8 bytes| little-endian   |
| `ref`  | managed reference       | 8 bytes| heap pointer    |

## 2. Instruction Set Architecture

### 2.1 Design Philosophy

MSIL operations mirror hardware concerns, not language constructs:

**What CPUs care about:**

- Control flow (branches, calls, returns)
- Arithmetic and bitwise operations
- Memory access (load/store, addressing modes)
- Type primitives (integers, floats, references)

**What MSIL abstracts:**

- Register file → operand stack
- Instruction decoding → bytecode interpretation
- Memory management → managed heap with GC

**Language constructs become library calls:**

- Lists → cons/car/cdr in stdlib
- Classes → vtable dispatch via call.virt
- Ranges/iterators → loops with branch instructions
- Pattern matching → branch on value tags

For example, a Musi `choice` pattern match compiles to:

```
; Load variant
ld.tag variant   ; Get variant tag
br.else case_some offset_else
; Handle Some case
...
br end_all
offset_else:
; Handle None case
...
end_all:
```

### 2.3 Opcode Encoding

MSIL uses two-byte opcodes:

- Primary opcodes: 0x00—0xEF (common operations)
- Extended opcodes: 0xF0—0xFF (control flow, exceptions)
- Reserve: 0xFE00—0xFEFF (future expansion)

### 2.3.1 Mnemonic Scheme

Mnemonics follow a categorical, consistent structure:

- `action.category[.area][.type]`
- `action`: `ld`, `st`, `br`, `call`, `new`, `conv`, `tail`, etc.
- `category`: `imm`, `loc`, `arg`, `mem`, `fld`, `elem`, `type`, `attr`, etc.
- `area` (optional): `addr`, `ind`, `virt`, `len`, `tag`, etc.
- `type` (optional): `i8`, `i16`, `n32`, `r64`, `ref`, etc.

No aliases are defined. If a value can be expressed via existing primitives, no dedicated opcode is added.

### 2.2 Stack Operations

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x00   | `nop`    | —        | No operation |
| 0x01   | `dup`    | —        | Duplicate top stack value |
| 0x02   | `drop`   | —        | Discard top stack value |
| 0x03   | `swap`   | —        | Swap top two values |

**Effect on stack**:

```
nop:   [...] → [...]
dup:   [v] → [v, v]
drop:  [v] → []
swap:  [v1, v2] → [v2, v1]
```

### 2.4 Constant Loading

Load immediate values onto operand stack:

| Opcode | Mnemonic | Operands | Value Range |
|--------|----------|----------|-------------|
| 0x10   | `ld.imm.i8`  | value:i8 | -128..127 |
| 0x11   | `ld.imm.i16` | value:i16| -32768..32767 |
| 0x12   | `ld.imm.i32` | value:i32| −2³¹..2³¹-1 |
| 0x13   | `ld.imm.i64` | value:i64| −2⁶³..2⁶³-1 |
| 0x14   | `ld.imm.n8`  | value:n8 | 0..255 |
| 0x15   | `ld.imm.n16` | value:n16| 0..65535 |
| 0x16   | `ld.imm.n32` | value:n32| 0..2³²-1 |
| 0x17   | `ld.imm.n64` | value:n64| 0..2⁶⁴-1 |
| 0x18   | `ld.imm.r32` | value:r32| IEEE single |
| 0x19   | `ld.imm.r64` | value:r64| IEEE double |
| 0x1A   | `ld.imm.str` | index:n32| constant pool |
| 0x1B   | `rsvd` | —      | reserved |
| 0x1C   | `rsvd` | —      | reserved |
| 0x1D   | `ld.imm.nil` | —        | `null` (not in Musi) |

Note: `Bool` is represented as `0`/`1`; use `ld.imm.i8` or `ld.imm.n8` for boolean literals.

**Effect on stack**:

```
ld.imm.i8(42):  [...] → [..., i32(42)]
ld.imm.str(3):  [...] → [..., ref(str)]
```

### 2.5 Local Variables

Local variable storage is per-frame; `index` encodes frame offset:

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x20   | `ld.loc` | index:n16 | Load local variable |
| 0x21   | `st.loc` | index:n16 | Store to local variable |
| 0x22   | `ld.loc.addr`| index:n16 | Load address of local |

**Effect on stack**:

```
ld.loc(0):  [...] → [..., value]
st.loc(0):  [..., value] → []
```

### 2.6 Arguments

Function arguments accessed via frame-relative indexing:

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x23   | `ld.arg` | index:n16 | Load argument |
| 0x24   | `st.arg` | index:n16 | Store to argument |
| 0x25   | `ld.arg.addr`| index:n16 | Load address of argument |

### 2.7 Memory Operations

Memory load/store through pointer addresses:

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x30   | `ld.mem.i8`  | Load signed 8-bit from memory address |
| 0x31   | `ld.mem.i16` | Load signed 16-bit from memory address |
| 0x32   | `ld.mem.i32` | Load signed 32-bit from memory address |
| 0x33   | `ld.mem.i64` | Load signed 64-bit from memory address |
| 0x34   | `ld.mem.n8`  | Load unsigned 8-bit from memory address |
| 0x35   | `ld.mem.n16` | Load unsigned 16-bit from memory address |
| 0x36   | `ld.mem.n32` | Load unsigned 32-bit from memory address |
| 0x37   | `ld.mem.n64` | Load unsigned 64-bit from memory address |
| 0x38   | `ld.mem.r32` | Load 32-bit float from memory address |
| 0x39   | `ld.mem.r64` | Load 64-bit float from memory address |
| 0x3A   | `ld.mem.ref` | Load reference from memory address |
| 0x3B   | `st.mem.i8`  | Store 8-bit to memory address |
| 0x3C   | `st.mem.i16` | Store 16-bit to memory address |
| 0x3D   | `st.mem.i32` | Store 32-bit to memory address |
| 0x3E   | `st.mem.i64` | Store 64-bit to memory address |
| 0x3F   | `st.mem.n8`  | Store unsigned 8-bit to memory address |
| 0x40   | `st.mem.n16` | Store unsigned 16-bit to memory address |
| 0x41   | `st.mem.n32` | Store unsigned 32-bit to memory address |
| 0x42   | `st.mem.n64` | Store unsigned 64-bit to memory address |
| 0x43   | `st.mem.r32` | Store 32-bit float to memory address |
| 0x44   | `st.mem.r64` | Store 64-bit float to memory address |
| 0x45   | `st.mem.ref` | Store reference to memory address |

**Effect on stack**:

```
ld.mem.i32:  [addr] → [value]
st.mem.i32:  [addr, value] → []
```

### 2.8 Arithmetic Operations

Integer and floating-point arithmetic (type-tagged operands):

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x50   | `add`    | Add (int or float, type inferred) |
| 0x51   | `sub`    | Subtract |
| 0x52   | `mul`    | Multiply |
| 0x53   | `div`    | Divide |
| 0x54   | `rem`    | Remainder (integer) |
| 0x55   | `neg`    | Negate |

### 2.9 Bitwise Operations

Bit manipulation on integers:

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x60   | `and`    | Bitwise AND |
| 0x61   | `or`     | Bitwise OR |
| 0x62   | `xor`    | Bitwise XOR |
| 0x63   | `not`    | Bitwise NOT |
| 0x64   | `shl`    | Shift left |
| 0x65   | `shr`    | Shift right (arithmetic) |
| 0x66   | `rol`    | Rotate left |
| 0x67   | `ror`    | Rotate right |

### 2.10 Comparison Operations

Comparison and equality checks:

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x70   | `eq`     | Equal (returns bool) |
| 0x71   | `neq`    | Not equal |
| 0x72   | `lt`     | Less than |
| 0x73   | `lte`    | Less than or equal |
| 0x74   | `gt`     | Greater than |
| 0x75   | `gte`    | Greater than or equal |

### 2.11 Control Flow

Branches, calls, and returns:

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x80   | `br`     | offset:i32| Unconditional branch (relative offset) |
| 0x81   | `br.true`| offset:i32| Branch if top of stack is true |
| 0x82   | `br.false`| offset:i32| Branch if top of stack is false |
| 0x85   | `ret`    | —        | Return from function |
| 0x86   | `call`   | index:n16| Call function by constant pool index |
| 0x87   | `call.ind`| — | Call function pointer on stack |
| 0x88   | `call.virt`| index:n16| Virtual call (instance method) |

### 2.12 Stack Operations

Additional stack manipulation:

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x94   | `pick`   | depth:n8 | Duplicate nth value from stack top |
| 0x95   | `roll`   | depth:n8 | Rotate n values on stack |

### 2.13 Object Operations

Object field access and allocation:

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xA0   | `new.obj` | type:n16 | Allocate new object |
| 0xA1   | `ld.fld`  | field:n16 | Load object field |
| 0xA2   | `st.fld`  | field:n16 | Store object field |
| 0xA3   | `ld.fld.addr` | field:n16 | Load field address |

**Effect on stack**:

```
new.obj: [...] → [..., ref(object)]
ld.fld:  [..., ref(object)] → [..., value]
st.fld:  [..., ref(object), value] → [...]
```

### 2.14 Array Operations

Array element access and allocation:

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xB0   | `new.arr` | type:n16, size:n32 | Allocate array |
| 0xB1   | `ld.elem` | type:n8 | Load array element |
| 0xB2   | `st.elem` | type:n8 | Store array element |
| 0xB3   | `ld.elem.addr` | type:n8 | Load element address |
| 0xB4   | `ld.arr.len` | — | Load array length |

**Effect on stack**:

```
new.arr: [...] → [..., ref(array)]
ld.elem: [..., ref(array), index] → [..., value]
st.elem: [..., ref(array), index, value] → [...]
```

### 2.15 Type Operations

Type operations for gradual typing and dynamic dispatch:

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xC0   | `ld.typ` | type:n16 | Load type object |
| 0xC1   | `is.typ` | type:n16 | Type check → bool |
| 0xC2   | `conv.typ` | type:n16 | Type conversion |
| 0xC3   | `conv.box` | type:n16 | Box value type |
| 0xC4   | `conv.unbox` | type:n16 | Unbox to value type |
| 0xC5   | `ld.typ.tag` | — | Get runtime type tag → i32 |

**Effect on stack**:

```
is.typ:  [..., value] → [..., bool]
conv.typ: [..., value] → [..., converted]
ld.typ.tag:   [..., value] → [..., type_tag]
```

### 2.16 Exception Handling (Extended Opcodes: 0xF0-0xFF)

Exception handling for language interop:

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xF0   | `throw` | — | Throw exception object |
| 0xF1   | `rethrow` | — | Rethrow current exception |
| 0xF2   | `try` | handler:i32 | Begin try block |
| 0xF3   | `catch` | type:n16 | Begin catch handler |
| 0xF4   | `finally` | — | Begin finally block |
| 0xF5   | `leave` | offset:i32 | Leave protected region |

**Effect on stack**:

```
throw: [..., exception] → [exception propagates]
try:   [...] → [...] (establishes exception handler)
```

### 2.17 Dynamic Operations (Optional)

Dynamic operations for fully dynamic languages:

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xD0   | `get.attr` | name:n32 | Get attribute by name |
| 0xD1   | `set.attr` | name:n32 | Set attribute by name |
| 0xD2   | `inv.dyn` | name:n32 | Dynamic method invocation |
| 0xD3   | `has.attr` | name:n32 | Check attribute existence → bool |

**Effect on stack**:

```
get.attr: [..., object] → [..., value]
set.attr: [..., object, value] → [...]
inv.dyn: [..., object, args...] → [..., result]
```

### 2.18 Tail Call Optimization

Tail call operations for functional languages:

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xE0   | `tail.call` | index:n16 | Tail-optimized call |
| 0xE1   | `tail.call.ind` | — | Tail-optimized indirect call |

**Effect on stack**:

```
tail.call: [..., args...] → [..., result] (reuses current frame)
```
