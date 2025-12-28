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
aux_version = n32;                           // version number
aux_flags = n32;                             // bit flags (reserved)
aux_const_pool_len = n32;                    // constant pool entry count
aux_symbol_table_len = n32;                  // symbol table entry count
aux_code_section_len = n32;                  // code section size in bytes
aux_mso_header = aux_magic, aux_version, aux_flags, aux_const_pool_len, aux_symbol_table_len, aux_code_section_len;
```

## Type Notation

- **i8/i16/i32/i64**: integers (signed)
- **n8/n16/n32/n64**: naturals (unsigned)
- **r16/r32/r64**: reals (IEEE-754 floats)
- **ref**: reference

## Instruction Set

### Stack Operations

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x00 | `nop` | no operation |
| 0x25 | `dup` | duplicate top stack value |
| 0x26 | `drop` | discard top stack value |
| 0x27 | `swap` | swap top two values |

### Constant Loading

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x20 | `ld.i32` | value:i32 | load 32-bit signed integer |
| 0x21 | `ld.i64` | value:i64 | load 64-bit signed integer |
| 0x24 | `ld.n32` | value:n32 | load 32-bit unsigned integer |
| 0x2F | `ld.n64` | value:n64 | load 64-bit unsigned integer |
| 0x2E | `ld.r32` | value:r32 | load 32-bit float |
| 0x30 | `ld.r64` | value:r64 | load 64-bit float |
| 0x72 | `ld.str` | index:n32 | load string from constant pool |
| 0x15 | `ld.true` | — | load boolean true |
| 0x16 | `ld.false` | — | load boolean false |

### Local Variables & Arguments

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x0E | `ld.loc` | index:n16 | load local variable |
| 0x0F | `st.loc` | index:n16 | store to local variable |
| 0x10 | `ld.loc.addr` | index:n16 | load address of local |
| 0x02 | `ld.arg` | index:n16 | load argument |
| 0x03 | `st.arg` | index:n16 | store to argument |
| 0x04 | `ld.arg.addr` | index:n16 | load address of argument |

### Pointer Operations

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x46 | `ld.ptr.i8` | load signed 8-bit through pointer |
| 0x48 | `ld.ptr.i16` | load signed 16-bit through pointer |
| 0x4A | `ld.ptr.i32` | load signed 32-bit through pointer |
| 0x4C | `ld.ptr.i64` | load signed 64-bit through pointer |
| 0x47 | `ld.ptr.n8` | load unsigned 8-bit through pointer |
| 0x49 | `ld.ptr.n16` | load unsigned 16-bit through pointer |
| 0x4B | `ld.ptr.n32` | load unsigned 32-bit through pointer |
| 0x4D | `ld.ptr.n64` | load unsigned 64-bit through pointer |
| 0x4E | `ld.ptr.r32` | load 32-bit float through pointer |
| 0x4F | `ld.ptr.r64` | load 64-bit float through pointer |
| 0x50 | `ld.ptr.ref` | load reference through pointer |
| 0x54 | `st.ptr.i32` | store 32-bit integer through pointer |
| 0x55 | `st.ptr.i64` | store 64-bit integer through pointer |
| 0x56 | `st.ptr.r32` | store 32-bit float through pointer |
| 0x57 | `st.ptr.r64` | store 64-bit float through pointer |
| 0x51 | `st.ptr.ref` | store reference through pointer |

### Object & Field Operations

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x73 | `new.obj` | init:n32 | allocate object, call initializer |
| 0x8D | `new.arr` | type:n32 | allocate array |
| 0x8E | `ld.len` | — | load array length |
| 0xA3 | `ld.elem` | — | load array element (index on stack) |
| 0xA4 | `st.elem` | — | store to array element |
| 0x8F | `ld.elem.addr` | — | load address of array element |
| 0x7B | `ld.fld` | field:n32 | load instance field |
| 0x7D | `st.fld` | field:n32 | store to instance field |
| 0x7C | `ld.fld.addr` | field:n32 | load address of field |

### Sum Type Operations

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xE0 | `new.sum` | type:n32, tag:n16 | construct sum variant |
| 0xE1 | `ld.tag` | — | load variant's tag |
| 0xE2 | `ld.sum.fld` | index:n16 | load field from variant |
| 0xE3 | `br.tag` | tag:n16, offset:i32 | branch if tag matches |

### Closure Operations

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xE4 | `new.cls` | fn:n32, captures:n16 | create closure |
| 0xE5 | `ld.capt` | index:n16 | load captured variable |
| 0xE6 | `st.capt` | index:n16 | store to captured variable |

### Control Flow

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x38 | `br` | offset:i32 | unconditional branch |
| 0x39 | `br.true` | offset:i32 | branch if true |
| 0x3A | `br.false` | offset:i32 | branch if false |
| 0x3B | `br.eq` | offset:i32 | branch if equal |
| 0x40 | `br.ne` | offset:i32 | branch if not equal |
| 0x3F | `br.lt` | offset:i32 | branch if less than |
| 0x3E | `br.le` | offset:i32 | branch if less or equal |
| 0x3D | `br.gt` | offset:i32 | branch if greater than |
| 0x3C | `br.ge` | offset:i32 | branch if greater or equal |
| 0x45 | `sw` | count:n32, offsets:i32[] | jump table dispatch |
| 0x2A | `ret` | — | return from function |

### Function Calls

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x28 | `call` | method:n32 | static method call |
| 0x29 | `call.ptr` | — | call via function pointer |
| 0x6E | `call.virt` | method:n32 | virtual dispatch |
| 0x6F | `call.dyn` | name:n32 | dynamic/late-bound call |
| 0xFE14 | `tail.call` | method:n32 | tail call (static) |
| 0xFE15 | `tail.call.virt` | method:n32 | tail call (virtual) |

### Type Operations

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x75 | `isinst` | type:n32 | type test (push boolean) |
| 0x74 | `asinst` | type:n32 | cast to type (throw on failure) |
| 0x76 | `asinst.try` | type:n32 | cast to type (Option on failure) |
| 0x8C | `box` | type:n32 | box value type to reference |
| 0xA5 | `unbox` | type:n32 | unbox reference to value type |
| 0xD0 | `ld.inst` | type:n32 | load type token |

### Conversions

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x69 | `conv.i32` | convert to 32-bit signed |
| 0x6A | `conv.i64` | convert to 64-bit signed |
| 0xD4 | `conv.n32` | convert to 32-bit unsigned |
| 0xD5 | `conv.n64` | convert to 64-bit unsigned |
| 0xB6 | `conv.r32` | convert to 32-bit float |
| 0xB7 | `conv.r64` | convert to 64-bit float |
| 0xD6 | `checked.conv.i32` | checked conversion (throw on overflow) |
| 0xD7 | `checked.conv.i64` | checked conversion |
| 0xD8 | `checked.conv.n32` | checked conversion |
| 0xD9 | `checked.conv.n64` | checked conversion |

### Arithmetic Operations

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x58 | `add` | addition |
| 0x59 | `sub` | subtraction |
| 0x5A | `mul` | multiplication |
| 0x5B | `div` | signed division |
| 0x5C | `div.un` | unsigned division |
| 0x5D | `rem` | signed remainder |
| 0x5E | `rem.un` | unsigned remainder |
| 0x65 | `neg` | arithmetic negation |
| 0xDA | `checked.add` | addition with overflow check |
| 0xDB | `checked.sub` | subtraction with overflow check |
| 0xDC | `checked.mul` | multiplication with overflow check |

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
| 0xC0 | `cmp.eq` | compare equal |
| 0xC1 | `cmp.ne` | compare not equal |
| 0xC2 | `cmp.gt` | compare greater (signed) |
| 0xC3 | `cmp.gt.un` | compare greater (unsigned) |
| 0xC4 | `cmp.lt` | compare less (signed) |
| 0xC5 | `cmp.lt.un` | compare less (unsigned) |
| 0xC6 | `cmp.ge` | compare greater or equal (signed) |
| 0xC7 | `cmp.le` | compare less or equal (signed) |

### Dynamic Language Support

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xF0 | `ld.dyn` | name:n32 | load dynamic property |
| 0xF1 | `st.dyn` | name:n32 | store dynamic property |
| 0xF2 | `has.dyn` | name:n32 | check if property exists |
| 0xF3 | `del.dyn` | name:n32 | delete dynamic property |

### Exception Handling (Interop Only)

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x7A | `throw` | throw exception |
| 0x7E | `rethrow` | rethrow current exception |
| 0xDD | `leave` | exit protected region |
| 0xDE | `endfinally` | end finally block |

```ebnf
aux_exception_handler = aux_try_block, aux_catch_block, aux_finally_block;
aux_try_block = aux_opcode_try, {aux_instr}, aux_opcode_end_try;
aux_catch_block = aux_opcode_catch, exception_type:n32, {aux_instr}, aux_opcode_end_catch;
aux_finally_block = aux_opcode_finally, {aux_instr}, aux_opcode_end_finally;
```

**Note**: Musi language does not use exceptions. These opcodes exist for interoperability with exception-based languages.

### Defer Blocks

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xDF | `defer` | block:n32 | register defer block |
| 0xE7 | `enddefer` | — | end defer block |

Defer blocks execute when current scope exits (normal return, break, or error).

### Memory Management

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0xE8 | `pin` | pin object for FFI |
| 0xE9 | `unpin` | unpin object |
| 0xEA | `alloca` | stack allocate |
| 0xEB | `sizeof` | get size of type |

### Optimization Hints

```ebnf
aux_optimization_hint = aux_inline_hint | aux_specialize_hint | aux_unroll_hint;
aux_inline_hint = aux_opcode_inline, method:n32;
aux_specialize_hint = aux_opcode_specialize, generic_method:n32, type_args:n32;
aux_unroll_hint = aux_opcode_unroll, loop:n32, unroll_factor:n8;
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
ld.arg 0       // load x
ld.arg 1       // load y
add            // add
ret            // return result
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
ld.arg 0       // load x
ld.i32 0       // load 0
br.ge L1       // branch if x >= 0
ld.arg 0       // load x
neg            // negate
ret            // return -x
L1:
ld.arg 0       // load x
ret            // return x
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
ld.loc.addr 0  // load address of x
ld.i32 42      // load 42
st.ptr.i32     // store through pointer
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
ld.loc 0       // load value
sw 3, [L0, L1, L2]
br Ldefault
L0: call a; br Lend
L1: call b; br Lend
L2: call c; br Lend
Ldefault: call d
Lend:
```

### Sum Type (Option)

```musi
val opt: ?Int32 := Some(42);
match opt {
case Some(x) => x,
case None => 0
};
```

**Bytecode:**

```text
// Construct Some(42)
ld.i32 42
new.sum Option, 1    // tag 1 = Some

// Match
ld.tag               // get tag
br.tag 0, Lnone      // if tag 0 (None), branch
ld.sum.fld 0         // load inner value
br Lend
Lnone:
ld.i32 0
Lend:
```

### Tail-Recursive Function

```musi
fn factorial(n: Int32, acc: Int32): Int32 {
  if n <= 1 { return acc; };
  return factorial(n - 1, n * acc);
};
```

**Bytecode:**

```text
ld.arg 0           // load n
ld.i32 1           // load 1
br.gt Lrecurse     // if n > 1, recurse
ld.arg 1           // load acc
ret                // return acc
Lrecurse:
ld.arg 0           // load n
ld.i32 1           // load 1
sub                // n - 1
ld.arg 0           // load n
ld.arg 1           // load acc
mul                // n * acc
tail.call factorial
```
