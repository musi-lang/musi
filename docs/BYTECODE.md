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
| 0x01 | `dup` | duplicate top stack value |
| 0x02 | `drop` | discard top stack value |
| 0x03 | `swap` | swap top two values |

### Constant Loading

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x10 | `ld.i8` | value:i8 | load 8-bit signed integer |
| 0x11 | `ld.i16` | value:i16 | load 16-bit signed integer |
| 0x12 | `ld.i32` | value:i32 | load 32-bit signed integer |
| 0x13 | `ld.i64` | value:i64 | load 64-bit signed integer |
| 0x14 | `ld.n8` | value:n8 | load 8-bit unsigned integer |
| 0x15 | `ld.n16` | value:n16 | load 16-bit unsigned integer |
| 0x16 | `ld.n32` | value:n32 | load 32-bit unsigned integer |
| 0x17 | `ld.n64` | value:n64 | load 64-bit unsigned integer |
| 0x18 | `ld.r32` | value:r32 | load 32-bit float |
| 0x19 | `ld.r64` | value:r64 | load 64-bit float |
| 0x1A | `ld.str` | index:n32 | load string from constant pool |
| 0x1B | `ld.true` | — | load boolean true |
| 0x1C | `ld.false` | — | load boolean false |
| 0x1D | `ld.nil` | — | load null/none reference |

### Local Variables & Arguments

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x20 | `ld.loc` | index:n16 | load local variable |
| 0x21 | `st.loc` | index:n16 | store to local variable |
| 0x22 | `ld.loc.addr` | index:n16 | load address of local |
| 0x23 | `ld.arg` | index:n16 | load argument |
| 0x24 | `st.arg` | index:n16 | store to argument |
| 0x25 | `ld.arg.addr` | index:n16 | load address of argument |

### Pointer Operations

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x30 | `ld.ptr.i8` | load signed 8-bit through pointer |
| 0x31 | `ld.ptr.i16` | load signed 16-bit through pointer |
| 0x32 | `ld.ptr.i32` | load signed 32-bit through pointer |
| 0x33 | `ld.ptr.i64` | load signed 64-bit through pointer |
| 0x34 | `ld.ptr.n8` | load unsigned 8-bit through pointer |
| 0x35 | `ld.ptr.n16` | load unsigned 16-bit through pointer |
| 0x36 | `ld.ptr.n32` | load unsigned 32-bit through pointer |
| 0x37 | `ld.ptr.n64` | load unsigned 64-bit through pointer |
| 0x38 | `ld.ptr.r32` | load 32-bit float through pointer |
| 0x39 | `ld.ptr.r64` | load 64-bit float through pointer |
| 0x3A | `ld.ptr.ref` | load reference through pointer |
| 0x3B | `st.ptr.i8` | store 8-bit through pointer |
| 0x3C | `st.ptr.i16` | store 16-bit through pointer |
| 0x3D | `st.ptr.i32` | store 32-bit through pointer |
| 0x3E | `st.ptr.i64` | store 64-bit through pointer |
| 0x3F | `st.ptr.n8` | store 8-bit unsigned through pointer |
| 0x40 | `st.ptr.n16` | store 16-bit unsigned through pointer |
| 0x41 | `st.ptr.n32` | store 32-bit unsigned through pointer |
| 0x42 | `st.ptr.n64` | store 64-bit unsigned through pointer |
| 0x43 | `st.ptr.r32` | store 32-bit float through pointer |
| 0x44 | `st.ptr.r64` | store 64-bit float through pointer |
| 0x45 | `st.ptr.ref` | store reference through pointer |

### Control Flow

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x50 | `br` | offset:i32 | unconditional branch |
| 0x51 | `br.true` | offset:i32 | branch if true |
| 0x52 | `br.false` | offset:i32 | branch if false |
| 0x53 | `br.eq` | offset:i32 | branch if equal |
| 0x54 | `br.ne` | offset:i32 | branch if not equal |
| 0x55 | `br.lt` | offset:i32 | branch if less than (signed) |
| 0x56 | `br.le` | offset:i32 | branch if less or equal (signed) |
| 0x57 | `br.gt` | offset:i32 | branch if greater than (signed) |
| 0x58 | `br.ge` | offset:i32 | branch if greater or equal (signed) |
| 0x59 | `br.lt.un` | offset:i32 | branch if less than (unsigned) |
| 0x5A | `br.le.un` | offset:i32 | branch if less or equal (unsigned) |
| 0x5B | `br.gt.un` | offset:i32 | branch if greater than (unsigned) |
| 0x5C | `br.ge.un` | offset:i32 | branch if greater or equal (unsigned) |
| 0x5D | `jmp` | default:i32, count:n32, [offsets:i32...] | switch/jump table dispatch |
| 0x5E | `ret` | — | return from function |
| 0x5F | `ret.val` | — | return with value from stack |

### Function Calls

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0x60 | `call` | method:n32 | static method call |
| 0x61 | `call.ptr` | — | call via function pointer |
| 0x62 | `call.virt` | method:n32 | virtual dispatch |
| 0x63 | `tail.call` | method:n32 | tail call (static) |
| 0x64 | `tail.call.virt` | method:n32 | tail call (virtual) |

### Arithmetic Operations

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x70 | `add` | addition |
| 0x71 | `sub` | subtraction |
| 0x72 | `mul` | multiplication |
| 0x73 | `div` | signed division |
| 0x74 | `div.un` | unsigned division |
| 0x75 | `rem` | signed remainder |
| 0x76 | `rem.un` | unsigned remainder |
| 0x77 | `neg` | arithmetic negation |
| 0x78 | `checked.add` | addition with overflow check |
| 0x79 | `checked.sub` | subtraction with overflow check |
| 0x7A | `checked.mul` | multiplication with overflow check |

### Bitwise Operations

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x80 | `and` | bitwise AND |
| 0x81 | `or` | bitwise OR |
| 0x82 | `xor` | bitwise XOR |
| 0x83 | `not` | bitwise NOT |
| 0x84 | `shl` | shift left |
| 0x85 | `shr` | shift right (signed) |
| 0x86 | `shr.un` | shift right (unsigned) |

### Conversions

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0x90 | `conv.i8` | convert to 8-bit signed |
| 0x91 | `conv.i16` | convert to 16-bit signed |
| 0x92 | `conv.i32` | convert to 32-bit signed |
| 0x93 | `conv.i64` | convert to 64-bit signed |
| 0x94 | `conv.n8` | convert to 8-bit unsigned |
| 0x95 | `conv.n16` | convert to 16-bit unsigned |
| 0x96 | `conv.n32` | convert to 32-bit unsigned |
| 0x97 | `conv.n64` | convert to 64-bit unsigned |
| 0x98 | `conv.r32` | convert to 32-bit float |
| 0x99 | `conv.r64` | convert to 64-bit float |
| 0x9A | `checked.conv.i32` | checked conversion (throw on overflow) |
| 0x9B | `checked.conv.i64` | checked conversion |
| 0x9C | `checked.conv.n32` | checked conversion |
| 0x9D | `checked.conv.n64` | checked conversion |

### Object & Field Operations

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xA0 | `new.obj` | init:n32 | allocate object, call initializer |
| 0xA1 | `new.arr` | type:n32 | allocate array |
| 0xA2 | `ld.len` | — | load array length |
| 0xA3 | `ld.elem` | — | load array element (index on stack) |
| 0xA4 | `st.elem` | — | store to array element |
| 0xA5 | `ld.elem.addr` | — | load address of array element |
| 0xA6 | `ld.fld` | field:n32 | load instance field |
| 0xA7 | `st.fld` | field:n32 | store to instance field |
| 0xA8 | `ld.fld.addr` | field:n32 | load address of field |

### Sum Type Operations

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xB0 | `new.sum` | type:n32, tag:n16 | construct sum variant |
| 0xB1 | `ld.tag` | — | load variant's tag |
| 0xB2 | `ld.sum.fld` | index:n16 | load field from variant |
| 0xB3 | `br.tag` | tag:n16, offset:i32 | branch if tag matches |

### Closure Operations

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xB4 | `new.cls` | fn:n32, captures:n16 | create closure |
| 0xB5 | `ld.capt` | index:n16 | load captured variable |
| 0xB6 | `st.capt` | index:n16 | store to captured variable |

### Type Operations

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xC0 | `typeof` | type:n32 | type test (push boolean) |
| 0xC1 | `cast` | type:n32 | cast to type (throw on failure) |
| 0xC2 | `cast.try` | type:n32 | cast to type (Option on failure) |
| 0xC3 | `box` | type:n32 | box value type to reference |
| 0xC4 | `unbox` | type:n32 | unbox reference to value type |
| 0xC5 | `nil.chk` | — | throw if null/none |

### Defer Blocks

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xD0 | `defer` | block:n32 | register defer block |
| 0xD1 | `enddefer` | — | end defer block |

Defer blocks execute when current scope exits (normal return, break, or error).

### Memory Management

| Opcode | Mnemonic | Description |
|--------|----------|-------------|
| 0xD2 | `pin` | pin object for FFI |
| 0xD3 | `unpin` | unpin object |
| 0xD4 | `alloca` | stack allocate |
| 0xD5 | `sizeof` | get size of type |

### Concurrency & Memory Ordering

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xE0 | `fence` | mode:n8 | memory fence/barrier |
| 0xE1 | `volatile.ld` | — | volatile load |
| 0xE2 | `volatile.st` | — | volatile store |
| 0xE3 | `atomic.add` | — | atomic add |
| 0xE4 | `atomic.cas` | — | compare-and-swap |
| 0xE5 | `atomic.load` | — | atomic load |
| 0xE6 | `atomic.store` | — | atomic store |

### Exception Handling (Interop Only)

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xF0 | `throw` | — | throw exception |
| 0xF1 | `rethrow` | — | rethrow current exception |
| 0xF2 | `leave` | offset:i32 | exit protected region, branch to offset |
| 0xF3 | `endfinally` | — | end finally block |
| 0xF4 | `try` | handler:n32 | begin try block |
| 0xF5 | `catch` | type:n32 | begin catch block |
| 0xF6 | `finally` | — | begin finally block |

**Note**: Musi language does not use exceptions. These opcodes exist for interoperability with exception-based languages.

### Extended Bytecode (0xFE prefix)

Extended instructions use two-byte opcodes: `0xFE` followed by extension byte.

#### Dynamic Language Support

| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| 0xFE00 | `ld.dyn` | name:n32 | load dynamic property |
| 0xFE01 | `st.dyn` | name:n32 | store dynamic property |
| 0xFE02 | `has.dyn` | name:n32 | check if property exists |
| 0xFE03 | `del.dyn` | name:n32 | delete dynamic property |
| 0xFE04 | `call.dyn` | name:n32 | dynamic/late-bound call |

**Note**: These are optional extensions for dynamic language implementations.

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
ret.val        // return result
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
ret.val        // return -x
L1:
ld.arg 0       // load x
ret.val        // return x
```

### Pointer Operation

```musi
unsafe {
  val ptr: VarPtr[Int32] := @x;
  ptr.store(42);

  val value := ptr.pointee;
};
```

**Bytecode:**

```text
ld.loc.addr 0  // load address of x (returns MutPtr[Int32])
ld.i32 42      // load 42
st.ptr.i32     // store through pointer

ld.loc.addr 0  // load address of x again
ld.ptr.i32     // load through pointer (ptr.pointee/ptr.load())
```

**Note**: Pointers in Musi use generic-style syntax (`Ptr[T]` for immutable, `VarPtr[T]` for mutable), similar to Swift's `UnsafePointer<T>` and `UnsafeMutablePointer<T>`. The `.pointee` property and `.store()` methods compile to `ld.ptr.*` and `st.ptr.*` bytecode instructions.

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
ld.loc 0                    // load value
sw Ldefault, 3, [L0, L1, L2]  // switch with default
L0: call a; br Lend
L1: call b; br Lend
L2: call c; br Lend
Ldefault: call d
Lend:
```

### Sum Type (Option)

```musi
val opt: Option[Int32] := Some(42);
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
ret.val            // return acc
Lrecurse:
ld.arg 0           // load n
ld.i32 1           // load 1
sub                // n - 1
ld.arg 0           // load n
ld.arg 1           // load acc
mul                // n * acc
tail.call factorial
```

### Labeled Loop with Break

```musi
#outer for i in 0..10 {
  for j in 0..10 {
    if should_exit {
      break #outer i;  // break outer loop with value
    };
  }
}
```

**Bytecode:**

```text
ld.i32 0
st.loc 0           // i := 0
Louter:
  ld.i32 0
  st.loc 1         // j := 0
  Linner:
    // ... should_exit check ...
    br.false Lskip
    ld.loc 0       // load i
    br Lendouter   // break #outer with value
  Lskip:
    // ... increment j ...
    br Linner
  Lendinner:
  // ... increment i ...
  br Louter
Lendouter:
```
