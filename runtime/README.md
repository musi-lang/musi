# Musi Runtime

A **100% safe Rust** implementation of the Musi Virtual Machine with ARC (Automatic Reference Counting) memory management.

## Architecture

### Safe Rust VM Design

This runtime demonstrates that a complete VM can be built **without any `unsafe` code**:

- **Memory Management**: Pure Rust ARC with `Rc<T>` and `RefCell<T>` for interior mutability
- **Foreign Function Interface**: Uses `extern "C"` safe blocks - no unsafe needed
- **Bytecode Execution**: Safe instruction dispatch with proper bounds checking
- **Value System**: Type-safe value representations with compile-time guarantees

### Key Components

- **Engine**: Main VM execution coordinator
- **Executor**: Instruction dispatch loop with safe opcode handling
- **Memory**: ARC-based garbage collection without raw pointers
- **Stack**: Type-safe value stack with bounds checking
- **Loader**: Safe bytecode file parsing and validation

## Usage

### Basic Execution

```bash
# Execute Musi bytecode
musi program.mso

# Verbose execution
musi -v program.mso

# Disassemble bytecode
musi disasm program.mso
```

### Error Reporting

The runtime provides CLR/Java-style error reporting:

```
Runtime Error: StackUnderflow at instruction 42
  at Program.main (program.mso:15:5)
```

## Memory Model

### Automatic Reference Counting

- Objects are reference-counted automatically
- No manual memory management required
- Safe deallocation when reference count reaches zero
- No dangling pointers or memory leaks

### Safe Abstractions

All potentially unsafe operations are wrapped in safe abstractions:

```rust
// Safe FFI - no unsafe block needed
extern "C" {
    fn write(fd: i32, data: *const u8, len: usize) -> i32;
}

// Safe memory allocation
let string = memory.allocate_string("Hello")?;  // Safe ARC
```

## Performance

### Zero-Copy Design

- Memory-mapped bytecode loading where possible
- Stack-based evaluation for optimal performance
- Compile-time constant folding in safe contexts

### Optimizations

- Instruction-level parallelism where safe
- JIT-ready architecture (future enhancement)
- Cache-friendly memory layout

## Safety Guarantees

### Compile-Time Safety

- All operations are type-checked at compile time
- No null pointer dereferences
- No buffer overflows (bounds checking everywhere)
- No data races in single-threaded execution

### Runtime Safety

- All array access is bounds-checked
- Stack operations prevent overflow/underflow
- Type conversions are validated
- Memory is automatically managed

## CLI Commands

### Standard Commands

```bash
musi run program.mso      # Execute bytecode
musi disasm program.mso    # Disassemble bytecode
musi info program.mso     # Show file information
musi version              # Show runtime version
```

### Options

```bash
-v, --verbose              # Verbose output
-s, --stats                # Execution statistics
--stack-size SIZE          # Custom stack size
--call-depth DEPTH         # Custom call depth
```

## Error Handling

### CLR-Style Errors

```
System.OverflowException: Stack overflow at instruction 123
  at Example.recursive_call (example.mso:42:12)

System.InvalidCastException: Cannot cast String to Int
  at Example.type_mismatch (example.mso:15:8)
```

### Structured Error Information

- Error type and location
- Call stack trace
- Instruction pointer position
- Source file reference

## Development

### Adding New Instructions

All instructions must be implemented safely:

```rust
// Safe instruction implementation
fn execute_newarray(&mut self, type_id: u32, size: usize) -> Result<Value> {
    // Bounds checking
    if size > MAX_ARRAY_SIZE {
        return Err(Fault::StackOverflow);
    }

    // Safe allocation
    let array = self.memory.allocate_array(Vec::with_capacity(size), type_id)?;

    Ok(Value::Array(array))
}
```

### Extending the Runtime

- Add new instruction opcodes in `instr.rs`
- Implement safe execution in `executor.rs`
- Add corresponding tests
- Update documentation

## Testing

### Unit Tests

```bash
cargo test
```

### Integration Tests

```bash
# Test with sample bytecode
cargo run --example test_program

# Run all test programs
cargo test --test integration
```

## Dependencies

- **Clap**: CLI argument parsing
- **Libc**: System call bindings (safe extern "C")
- **Hashbrown**: Fast hash maps
- **Bytes**: Safe byte slice handling

## License

MIT OR Apache-2.0
