# Musi Bytecode Specification

## Abstract

MSIL (Musi Stack-based Intermediate Language) is a general-purpose bytecode format designed for efficient execution. Inspired by CIL and Java bytecode but optimized for performance and hardware proximity. The instruction set focuses on what CPUs care about: control flow, arithmetic, memory access, and type-aware operations without language-specific constructs.

**Design Principles**:

- **Hardware-inspired**: General-purpose opcodes similar to CPU instruction sets
- **Type-tagged values**: Operands carry type tags for safety without runtime checks
- **Stack-based**: Simple, fast evaluation model with minimal state
- **No language assumptions**: No built-in support for lists, classes, ranges, etc. — these are library constructs

**Compiler Layer**: musi_bytecode (Layer 5) ← musi_types (Layer 4) ← musi_syntax (Layer 3)

## 1. File Format (.mso)

### 1.1 Module Structure

Compiled Musi modules use the `.mso` (Musi Stack Object) format:

```ebnf
module ::= header constant_pool symbol_table code_section
```

```text
+-------------------+  ← 32 bytes
|      Header       |  magic, version, flags, section sizes
+-------------------+  ← header.const_pool_len * 8 bytes
|   Constant Pool   |  string/numeric literals
+-------------------+  ← header.symbol_table_len * 16 bytes
|   Symbol  Table   |  function/type/field names
+-------------------+  ← header.code_section_len bytes
|   Code  Section   |  bytecode instructions
+-------------------+
```

### 1.2 Header Format

```hex
Offset  Size  Field
------  ----  -----
0x00    4     magic: 0x4D555349 (ASCII "MUSI")
0x04    4     version: u32 (format version)
0x08    4     flags: u32 (bit flags, reserved)
0x0C    4     const_pool_len: u32 (entry count)
0x10    4     symbol_table_len: u32 (entry count)
0x14    4     code_section_len: u32 (byte length)
0x18    4     entry_point: u32 (symbol table index)
0x1C    12    reserved (zero)
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

- Primary opcodes: 0x00–0xEF (common operations)
- Extended opcodes: 0xF0–0xFF (control flow, exceptions)
- Reserve: 0xFE00–0xFEFF (future expansion)

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
| 0x10   | `ld.i8`  | value:i8 | -128..127 |
| 0x11   | `ld.i16` | value:i16| -32768..32767 |
| 0x12   | `ld.i32` | value:i32| –2³¹..2³¹-1 |
| 0x13   | `ld.i64` | value:i64| –2⁶³..2⁶³-1 |
| 0x14   | `ld.n8`  | value:n8 | 0..255 |
| 0x15   | `ld.n16` | value:n16| 0..65535 |
| 0x16   | `ld.n32` | value:n32| 0..2³²-1 |
| 0x17   | `ld.n64` | value:n64| 0..2⁶⁴-1 |
| 0x18   | `ld.r32` | value:r32| IEEE single |
| 0x19   | `ld.r64` | value:r64| IEEE double |
| 0x1A   | `ld.str` | index:n32| constant pool |
| 0x1B   | `ld.true`| —        | `true` |
| 0x1C   | `ld.false`| —      | `false` |
| 0x1D   | `ld.nil` | —        | `null` |

**Effect on stack**:

```
ld.i8(42):  [...] → [..., i32(42)]
ld.str(3):  [...] → [..., ref(str)]
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

### 2.7 Pointer Operations

Memory load/store through pointer addresses:

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x30   | `ld.i8.byt`| Load signed 8-bit, byte address |
| 0x31   | `ld.i16.byt`| Load signed 16-bit, byte address |
| 0x32   | `ld.i32.byt`| Load signed 32-bit, byte address |
| 0x33   | `ld.i64.byt`| Load signed 64-bit, byte address |
| 0x34   | `ld.n8.byt`| Load unsigned 8-bit, byte address |
| 0x35   | `ld.n16.byt`| Load unsigned 16-bit, byte address |
| 0x36   | `ld.n32.byt`| Load unsigned 32-bit, byte address |
| 0x37   | `ld.n64.byt`| Load unsigned 64-bit, byte address |
| 0x38   | `ld.r32.byt`| Load 32-bit float, byte address |
| 0x39   | `ld.r64.byt`| Load 64-bit float, byte address |
| 0x3A   | `ld.ref.byt`| Load reference, byte address |
| 0x3B   | `st.i8.byt`| Store 8-bit, byte address |
| 0x3C   | `st.i16.byt`| Store 16-bit, byte address |
| 0x3D   | `st.i32.byt`| Store 32-bit, byte address |
| 0x3E   | `st.i64.byt`| Store 64-bit, byte address |
| 0x3F   | `st.n8.byt`| Store unsigned 8-bit, byte address |
| 0x40   | `st.n16.byt`| Store unsigned 16-bit, byte address |
| 0x41   | `st.n32.byt`| Store unsigned 32-bit, byte address |
| 0x42   | `st.n64.byt`| Store unsigned 64-bit, byte address |
| 0x43   | `st.r32.byt`| Store 32-bit float, byte address |
| 0x44   | `st.r64.byt`| Store 64-bit float, byte address |
| 0x45   | `st.ref.byt`| Store reference, byte address |

**Effect on stack**:

```
ld.i32.byt:  [addr] → [value]
st.i32.byt:  [addr, value] → []
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
| 0x56   | `abs`    | Absolute value |
| 0x57   | `floor`  | Floor (float) |
| 0x58   | `ceil`   | Ceiling (float) |
| 0x59   | `round`  | Round to nearest |
| 0x5A   | `trunc`  | Truncate toward zero |

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
| 0x83   | `br.null`| offset:i32| Branch if top of stack is null |
| 0x84   | `br.notnull`| offset:i32| Branch if top of stack is not null |
| 0x85   | `ret`    | —        | Return from function |
| 0x86   | `call`   | index:n16| Call function by constant pool index |
| 0x87   | `call.indirect`| — | Call function pointer on stack |
| 0x88   | `call.virt`| index:n16| Virtual call (instance method) |

### 2.12 Stack Operations

Additional stack manipulation:

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x90   | `push`   | value     | Push value onto stack (optimized locals) |
| 0x91   | `pop`    | —        | Discard top of stack |
| 0x92   | `dup`    | —        | Duplicate top of stack |
| 0x93   | `swap`   | —        | Swap top two values |
| 0x94   | `pick`   | depth:n8 | Duplicate nth value from stack top |
| 0x95   | `roll`   | depth:n8 | Rotate n values on stack |

## 3. Language Constructs to Bytecode

### 3.1 Pattern Matching

Musi `choice` pattern matching compiles to tag-based dispatch:

**Source:**

```musi
match opt {
  Some(x) => x,
  None => 0
}
```

**Bytecode:**

```
ld.loc opt           ; load opt onto stack
ld.tag               ; get variant tag from value
ld.i8 0              ; Some variant index
eq
br.false handle_none ; branch if not Some

handle_some:
ld.loc opt
ld.field 0           ; extract payload
br end_match

handle_none:
ld.i32 0

end_match:
```

### 3.2 Method Calls (Virtual Dispatch)

Instance method calls compile to vtable indirection:

**Source:**

```musi
val Drawable := interface {
  val Point;
  draw: (self: Point) -> Unit
};

val Point := record {
  x: Int32,
  y: Int32,
  draw: (self: Point) -> Unit => (self) => ...
};

val p: Point := ...
p.draw();
```

**Bytecode:**

```
ld.loc p
ld.vtable           ; load vtable pointer from object
ld.ptr 0            ; load method offset for 'draw'
call.indirect       ; call method via vtable
```

### 3.3 List Operations

List cons/car/cdr are library functions, not bytecode:

**Source:**

```musi
val lst := 1 :: [2, 3];
val head := lst.head;
```

**Bytecode:**

```
ld.i32 1
ld.i32 2
ld.i32 3
call List.cons      ; library function, not opcode
...
dup
call List.head      ; library function
```

### 3.4 Conditional Pattern Binding

Pattern binding in `if` statements:

**Source:**

```musi
if Some(x) := opt {
  x + 1
} else {
  0
}
```

**Bytecode:**

```
ld.loc opt
ld.tag
ld.i8 0              ; Some variant
br.false else_branch
ld.loc opt
st.loc x            ; bind x
ld.loc x
ld.i32 1
add
br end_if

else_branch:
ld.i32 0

end_if:
```

## 4. Implementation Notes

### 4.1 Foreign Function Interface

Layer 6 (musi_vm) exports C-compatible symbols:

```c
typedef struct MusiVM MusiVM;
typedef struct MusiValue { uint64_t bits; } MusiValue;

MusiVM* musi_vm_create(void);
void musi_vm_destroy(MusiVM* vm);
int musi_vm_load_bytecode(MusiVM* vm, const uint8_t* data, size_t len);
int musi_vm_run(MusiVM* vm);
MusiValue musi_vm_call_function(MusiVM* vm, const char* name, ...);
```

See `RUNTIME.md` for VM embedding details.

### 4.2 Implementation Layer Reference

| Concept | Layer | Crate | File |
|---------|-------|-------|------|
| Bytecode module | 5 | musi_bytecode | `src/module.rs` |
| Emission context | 5 | musi_bytecode | `src/emit.rs` |
| Instruction encoding | 5 | musi_bytecode | `src/opcode.rs` |
| VM execution | 6 | musi_vm | `src/vm.rs` |
| Value system | 6 | musi_vm | `src/value.rs` |

## 5. Verifiability

Bytecode modules contain sufficient structure for static verification:

1. **Stack depth**: Each instruction's net stack effect is computable
2. **Control flow**: Jump targets are within code section bounds
3. **Symbol resolution**: All identifiers resolve in symbol table
4. **Type safety**: Runtime type tags prevent memory unsafety

## References

- [ARCHITECTURE.md](./ARCHITECTURE.md): Compiler architecture and layer model
- [RUNTIME.md](./RUNTIME.md): Virtual machine implementation
- `grammar.ebnf`: Source language grammar specification
