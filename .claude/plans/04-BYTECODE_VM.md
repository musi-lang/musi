# Phase 4 — Bytecode Format + VM Core

**Crate:** `musi_codegen`, `musi_vm`
**Goal:** Define .mso binary format, implement a minimal VM that can execute a hand-built bytecode program.
**Dependencies:** Phase 1 (musi_shared)

---

## Deliverables

### Opcode Enum (Initial Subset)

```
Opcode =
  // Load/store
  | ld.imm.i64(i64)       // push immediate int
  | ld.imm.f64(f64)       // push immediate float
  | ld.imm.bool(bool)     // push immediate bool
  | ld.imm.unit           // push unit
  | ld.const(u16)         // push from const pool by index
  | ld.loc(u16)           // push local variable
  | st.loc(u16)           // pop into local variable

  // Control
  | call(u16)             // call function by index (arg count in function table)
  | ret                   // return from function
  | nop                   // no operation
  | drop                  // discard top of stack
  | halt                  // terminate execution
```

### .mso Container Format

Binary format, little-endian.

```
Header:
  magic: [u8; 4] = b"MUSI"
  version: u16 = 1
  flags: u16 = 0

Sections (length-prefixed):
  1. Const Pool
     count: u32
     entries: [ConstEntry]
       ConstEntry = tag(u8) + payload
         0x01 = Int(i64)
         0x02 = Float(f64)
         0x03 = String(len: u32, bytes: [u8])
         0x04 = Bool(u8)

  2. Symbol Table
     count: u32
     entries: [SymbolEntry]
       SymbolEntry = {
         name_len: u16,
         name: [u8],
         flags: u8,          // bit 0: native, bit 1: export
         intrinsic_id: u16,  // valid when native flag set, 0xFFFF = none
       }

  3. Function Table
     count: u32
     entries: [FunctionEntry]
       FunctionEntry = {
         symbol_idx: u16,    // index into symbol table
         param_count: u8,
         local_count: u16,
         code_offset: u32,   // byte offset into code section
         code_length: u32,
       }

  4. Code Section
     length: u32
     bytes: [u8]             // concatenated function bytecodes
```

### Value Enum (Runtime)

```
Value =
  | Int(i64)
  | Float(f64)
  | Bool(bool)
  | String(Rc<str>)
  | Unit
```

### VM Architecture

```
CallFrame = {
  function_idx: u16,
  pc: usize,              // program counter within function's code
  locals: Vec<Value>,     // local variable slots
  stack_base: usize,      // operand stack base for this frame
}

Vm = {
  stack: Vec<Value>,       // operand stack
  frames: Vec<CallFrame>,  // call stack
  module: Module,          // loaded .mso data
  natives: NativeRegistry, // registered native function handlers
}
```

**Dispatch loop (pseudo-code):**
```
fn run():
  loop:
    frame = current_frame()
    op = decode_opcode(frame.pc)
    frame.pc += op.size()
    match op:
      ld.imm.i64(v) → stack.push(Int(v))
      ld.const(idx) → stack.push(const_pool[idx].to_value())
      ld.loc(idx)   → stack.push(frame.locals[idx].clone())
      st.loc(idx)   → frame.locals[idx] = stack.pop()
      call(fn_idx)  →
        func = function_table[fn_idx]
        if symbol_table[func.symbol_idx].is_native:
          dispatch_native(func, ...)
        else:
          push new CallFrame, transfer args to locals
      ret →
        result = stack.pop()
        pop frame
        stack.truncate(frame.stack_base)
        stack.push(result)
        if no frames left: return result
      drop → stack.pop()
      halt → return stack.top_or_unit()
      nop  → continue
```

### Native Function Interface

**No magic principle:** The VM doesn't know any function names. It reads the symbol table.

```
IntrinsicId = u16

NativeRegistry = {
  handlers: HashMap<IntrinsicId, fn(&mut Vm, &[Value]) -> Value>,
}
```

- .mso declares a function with `native` flag + `intrinsic_id`.
- On `call`, VM checks native flag → looks up `intrinsic_id` in registry → calls handler.
- The registry is populated at VM startup with built-in intrinsics.
- Initial intrinsics: `writeln` (id=1), `write` (id=2).

### Serialization / Deserialization

- `Module::serialize(&self) → Vec<u8>` — write .mso bytes.
- `Module::deserialize(bytes: &[u8]) → Result<Module>` — parse .mso bytes.
- Validate magic, version, section bounds.

---

## Milestone

1. Hand-build an .mso with:
   - Const pool: one String entry "Hello, world!"
   - Symbol table: one entry "writeln" with native flag, intrinsic_id=1
   - Function table: one entry for `main` (no params, no locals, code that loads const string + calls writeln + halt)
2. Serialize → deserialize round-trip → identical module.
3. VM executes → stdout shows "Hello, world!".
4. `cargo test -p musi_codegen -p musi_vm` passes.
