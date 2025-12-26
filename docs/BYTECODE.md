# Musi Bytecode Specification

## File Format (.mso)

### Structure

```text
+-------------------+
|      Header       |  magic, version, flags, section sizes
+-------------------+
|   Constant Pool   |  string/numeric literals
+-------------------+
|   Symbol  Table   |  function/type/field names
+-------------------+
|   Code  Section   |  bytecode instructions
+-------------------+
```

### Header Format

```ebnf
aux_magic = "0x4D555349";                    // ASCII "MUSI"
aux_version = u32;                           // version number
aux_flags = u32;                             // bit flags (reserved)
aux_const_pool_len = u32;                    // constant pool entry count
aux_symbol_table_len = u32;                  // symbol table entry count
aux_code_section_len = u32;                  // code section size in bytes
aux_mso_header = aux_magic, aux_version, aux_flags, aux_const_pool_len, aux_symbol_table_len, aux_code_section_len;
```

## Type Notation

- **i1/i2/i4/i8**: integral (signed integers, 8/16/32/64-bit)
- **n1/n2/n4/n8**: natural (unsigned integers, 8/16/32/64-bit)
- **r2/r4/r8**: real (IEEE-754 binary floats, 16/32/64-bit)
- **ref**: reference/pointer

## Instruction Set

### Control Flow

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x00 | `nop` | — | no operation |
| 0x38 | `br` | offset:i32 | unconditional branch |
| 0x39 | `brtrue` | offset:i32 | branch if true |
| 0x3A | `brfalse` | offset:i32 | branch if false |
| 0x3B | `beq` | offset:i32 | branch if equal |
| 0x40 | `bne` | offset:i32 | branch if not equal |
| 0x3F | `blt` | offset:i32 | branch if less than |
| 0x3E | `ble` | offset:i32 | branch if less or equal |
| 0x3D | `bgt` | offset:i32 | branch if greater than |
| 0x3C | `bge` | offset:i32 | branch if greater or equal |
| 0x45 | `switch` | count:u32, offsets:i32[] | jump table dispatch |
| 0x2A | `ret` | — | return from function |
| 0x7A | `throw` | — | throw exception (interop only) |
| 0x7B | `rethrow` | — | rethrow current exception |

### Stack Operations

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x25 | `dup` | duplicate top stack value |
| 0x26 | `pop` | discard top stack value |

### Constant Loading

**Pattern**: `ldc.<type>` loads typed constant onto stack

| Opcode | Mnemonic | Operands | Type |
|--------|----------|----------|------|
| 0x14 | `ldnull` | — | null reference (interop only) |
| 0x20 | `ldc.i4` | value:i32 | 32-bit signed integer |
| 0x21 | `ldc.i8` | value:i64 | 64-bit signed integer |
| 0x24 | `ldc.n4` | value:u32 | 32-bit unsigned integer |
| 0x2F | `ldc.n8` | value:u64 | 64-bit unsigned integer |
| 0x2E | `ldc.r4` | value:f32 | 32-bit binary float |
| 0x30 | `ldc.r8` | value:f64 | 64-bit binary float |
| 0x72 | `ldstr` | index:u32 | string from constant pool |

### Memory Operations

**Local variables and arguments:**

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x0E | `ldloc` | index:u16 | load local variable |
| 0x0F | `stloc` | index:u16 | store to local variable |
| 0x10 | `ldloca` | index:u16 | load address of local |
| 0x02 | `ldarg` | index:u16 | load argument |
| 0x03 | `starg` | index:u16 | store to argument |
| 0x04 | `ldarga` | index:u16 | load address of argument |

### Pointer Operations

**Pattern**: `ldind.<type>` loads value through pointer, `stind.<type>` stores through pointer

| Opcode | Mnemonic | Type | Direction |
|--------|----------|------|-----------|
| 0x46 | `ldind.i1` | signed 8-bit | load |
| 0x48 | `ldind.i2` | signed 16-bit | load |
| 0x4A | `ldind.i4` | signed 32-bit | load |
| 0x4C | `ldind.i8` | signed 64-bit | load |
| 0x47 | `ldind.n1` | unsigned 8-bit | load |
| 0x49 | `ldind.n2` | unsigned 16-bit | load |
| 0x4B | `ldind.n4` | unsigned 32-bit | load |
| 0x4D | `ldind.n8` | unsigned 64-bit | load |
| 0x4E | `ldind.r4` | 32-bit float | load |
| 0x4F | `ldind.r8` | 64-bit float | load |
| 0x50 | `ldind.ref` | reference | load |
| 0x54 | `stind.i4` | 32-bit integer | store |
| 0x55 | `stind.i8` | 64-bit integer | store |
| 0x56 | `stind.r4` | 32-bit float | store |
| 0x57 | `stind.r8` | 64-bit float | store |
| 0x51 | `stind.ref` | reference | store |

### Object Operations

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x73 | `newobj` | init:u32 | allocate object, call designated initializer |
| 0x8D | `newarr` | type:u32 | allocate array |
| 0x8E | `ldlen` | — | load array length |
| 0xA3 | `ldelem` | — | load array element (index on stack) |
| 0xA4 | `stelem` | — | store to array element |
| 0x8F | `ldelema` | — | load address of array element |
| 0x7B | `ldfld` | field:u32 | load instance field |
| 0x7D | `stfld` | field:u32 | store to instance field |
| 0x7C | `ldflda` | field:u32 | load address of field |

### Function Calls

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x28 | `call` | method:u32 | call static method |
| 0x29 | `callind` | — | call via function pointer |

### Type Operations

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x75 | `isinst` | type:u32 | type test (push boolean) |
| 0x74 | `asinst` | type:u32 | cast to type (throw on failure) |
| 0x8C | `box` | type:u32 | box value type to reference |
| 0xA5 | `unbox.any` | type:u32 | unbox reference to value type |

**Conversions** (pattern: `conv.<target_type>`):

| Opcode | Mnemonic | Target Type |
|--------|----------|-------------|
| 0x69 | `conv.i4` | 32-bit signed integer |
| 0x6A | `conv.i8` | 64-bit signed integer |
| 0xD4 | `conv.n4` | 32-bit unsigned integer |
| 0xD5 | `conv.n8` | 64-bit unsigned integer |
| 0xB6 | `conv.r4` | 32-bit binary float |
| 0xB7 | `conv.r8` | 64-bit binary float |

### Arithmetic Operations

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x58 | `add` | addition |
| 0xD6 | `add.ovf` | addition with overflow check |
| 0x59 | `sub` | subtraction |
| 0xD8 | `sub.ovf` | subtraction with overflow check |
| 0x5A | `mul` | multiplication |
| 0xDA | `mul.ovf` | multiplication with overflow check |
| 0x5B | `div` | signed division |
| 0x5C | `div.un` | unsigned division |
| 0x5D | `rem` | signed remainder |
| 0x5E | `rem.un` | unsigned remainder |
| 0x65 | `neg` | arithmetic negation |

### Bitwise Operations

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x5F | `and` | bitwise AND |
| 0x60 | `or` | bitwise OR |
| 0x61 | `xor` | bitwise XOR |
| 0x66 | `not` | bitwise NOT |
| 0x62 | `shl` | shift left |
| 0x63 | `shr` | shift right (signed) |
| 0x64 | `shr.un` | shift right (unsigned) |

### Comparison Operations

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0xFE | `ceq` | compare equal |
| 0xC2 | `cgt` | compare greater (signed) |
| 0xC3 | `cgt.un` | compare greater (unsigned) |
| 0xC4 | `clt` | compare less (signed) |
| 0xC5 | `clt.un` | compare less (unsigned) |

### Memory Management

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0xDF | `pin` | pin object for FFI |
| 0xE0 | `unpin` | unpin object |

## Exception Handling (Interop Only)

```ebnf
aux_exception_handler = aux_try_block, aux_catch_block, aux_finally_block;
aux_try_block = aux_opcode_try, {aux_instr}, aux_opcode_end_try;
aux_catch_block = aux_opcode_catch, exception_type:u32, {aux_instr}, aux_opcode_end_catch;
aux_finally_block = aux_opcode_finally, {aux_instr}, aux_opcode_end_finally;
```

**Note**: Musi language does not use exceptions. These opcodes exist for interoperability with exception-based languages.

## Defer Blocks

```ebnf
aux_defer_instr = aux_opcode_defer, defer_block:u32;
aux_defer_block = {aux_instr}, aux_opcode_end_defer;
```

Defer blocks execute when current scope exits (normal return, break, or error).

## Optimization Hints

```ebnf
aux_optimization_hint = aux_inline_hint | aux_specialize_hint | aux_unroll_hint;
aux_inline_hint = aux_opcode_inline, method:u32;
aux_specialize_hint = aux_opcode_specialize, generic_method:u32, type_args:u32;
aux_unroll_hint = aux_opcode_unroll, loop:u32, unroll_factor:u8;
```

**Purpose**: Optional metadata for JIT/AOT compilers. Does not affect semantics.

## Examples

### Function Call

```musi
fn add(x: Int32, y: Int32): Int32 {
  return x + y;
};
```

**Bytecode:**

```text
ldarg 0      // load x
ldarg 1      // load y
add          // add
ret          // return result
```

### Conditional Branch

```musi
fn abs(x: Int32): Int32 {
  if x < 0 { return -x; };
  x
};
```

**Bytecode:**

```text
ldarg 0      // load x
ldc.i4 0     // load 0
bge L1       // branch if x >= 0
ldarg 0      // load x
neg          // negate
ret          // return -x
L1:
ldarg 0      // load x
ret          // return x
```

### Pointer Operation

```musi
unsafe {
  val ptr: ^Int32 := @x;
  ptr.^ <- 42;
};
```

**Bytecode:**

```text
ldloca 0     // load address of x
ldc.i4 42    // load 42
stind.i4     // store through pointer
```

### Pattern Match to Switch

```musi
match value {
case 0 => a(),
case 1 => b(),
case 2 => c(),
case _ => d()
};
```

**Bytecode:**

```text
ldloc 0      // load value
switch 3, [L0, L1, L2]
br Ldefault
L0: call a; br Lend
L1: call b; br Lend
L2: call c; br Lend
Ldefault: call d
Lend:
```
