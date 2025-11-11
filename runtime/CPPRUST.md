# Cpprust: C++-style Rust Subset

Cpprust keeps Rust syntax while leaning into C++-style RAII and explicit unsafe programming.

## Guiding Rules

1. Mark every function `unsafe`, so callers never assume hidden guarantees.
2. Borrow with `&T` or `&mut T`, but expect callers to uphold validity like C++ references.
3. Use Cargo with a strict clippy profile to guard Cpprust habits.
4. Stay out of `std`; pair `alloc` and `libc` to build RAII containers.
5. Stick to Rust 2021 with rustc 1.56.0 or later.
6. Design structs as pure `Copy` pods or pure RAII owners, never both.
7. Expose items publicly unless there is a strong reason not to.

## Type Palette

### RAII smart pointers

- `Box<T>` mirrors C++ `std::unique_ptr<T>` with single ownership and drop on scope exit.
- `Rc<T>` behaves like `std::shared_ptr<T>` with non-threaded reference counts.
- `Arc<T>` offers the atomic equivalent of `std::shared_ptr<T>` for cross-thread sharing.

### RAII containers

- `Vec<T>` tracks heap arrays with automatic teardown.
- `String` stores owned UTF-8 text with RAII cleanup.
- `BTreeMap<K, V>` matches ordered associative maps.
- `BTreeSet<T>` matches ordered sets.

### Primitive building blocks

- `[T; N]` mimics `std::array<T, N>` on the stack.
- `(T, U)` mirrors `std::pair<T, U>`.
- `&[T]` works as a span or view with borrowed storage.
- `&str` works as a UTF-8 view.

### Raw pointers

- `*const T` equals `const T*` for read-only pointers.
- `*mut T` equals `T*` for writable pointers.
- Lifetimes stay unchecked; callers must handle null or dangling values.

## Disallowed Patterns

- Avoid `std::*`; rely on `alloc::*` or `libc::*` instead.
- Skip explicit lifetime parameters in public signatures; use elision rules.
- Ban lifetime-bound structs such as `struct Foo<'a>` in exposed APIs.
- Ban traits with lifetime parameters in exposed APIs.
- Keep borrowing-heavy contracts private.

## Memory Management Playbook

### Stack allocation (C++-style automatic objects)

```rust
#[derive(Clone, Copy)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

unsafe fn use_point() {
    let p = Point { x: 10, y: 20 };  // lives on stack and drops at scope end
}
```

### Heap allocation (unique ownership)

```rust
pub struct Node {
    pub value: i32,
    pub next: Option<Box<Node>>,  // owns successors and cleans up automatically
}

unsafe fn create_list() -> Box<Node> {
    Box::new(Node {
        value: 42,
        next: None,
    })  // caller receives ownership and drop responsibility
}
```

### Reference counting (shared ownership)

```rust
use alloc::rc::Rc;

pub struct Shared {
    pub data: Rc<Vec<i32>>,  // shares heap storage thru ref counts
}

unsafe fn share_data(s: *const Shared) -> Rc<Vec<i32>> {
    (*s).data.clone()  // bumps count on return; drop balances it
}
```

### Dynamic arrays (vector semantics)

```rust
unsafe fn build_array() -> Vec<i32> {
    let mut v = Vec::with_capacity(100);
    for i in 0..100 {
        v.push(i);
    }
    v  // ownership rets and drop frees buffer
}
```

## Signature Templates

### Allowed forms

```rust
// ok for const refe style access
unsafe fn process(data: &[u8]) -> &str { ... }

// ok for mutable ref style access
unsafe fn modify(x: &mut i32) { ... }

// ok for raw ptr access when needed
unsafe fn low_level(ptr: *mut u8, len: usize) { ... }
```

### Forbidden forms

```rust
// BAD: publishes explicit lifetime param
unsafe fn process<'a>(data: &'a [u8]) -> &'a str { ... }

// GOOD: elided lifetimes keep sig simple
unsafe fn process(data: &[u8]) -> &str { ... }
```

## Struct Design

### Copy payloads (plain old data)

```rust
#[derive(Clone, Copy)]
pub struct Value {
    pub tag: u8,
    pub data: i64,
}
```

### RAII owners (resource managers)

```rust
pub struct VM {
    pub stack: Vec<Value>,      // frees stack slots when VM drops
    pub frames: Vec<Frame>,     // releases frame storage automatically
    pub code: Box<[u8]>,        // owns code buffer lifetime
}

impl Drop for VM {
    fn drop(&mut self) {
        // custom cleanup hooks fit here if required
    }
}
```

### Mixed payloads (avoid)

```rust
// BAD: mixes Copy fields w/ RAII owners in 1 struct
pub struct Bad {
    pub x: i32,
    pub data: Vec<i32>,
}
```

## Tooling Setup

### Clippy guard rails

```toml
[lints.clippy]
missing_safety_doc = "deny"
undocumented_unsafe_blocks = "deny"
multiple_unsafe_ops_per_block = "deny"
not_unsafe_ptr_arg_deref = "allow"
```

### Build profile

```toml
[dependencies]
libc = { version = "0.2.177", default-features = false }

[profile.release]
opt-level = 3
lto = true
codegen-units = 1
panic = "abort"
strip = true
```

## Pitfalls to Watch

### Dangling pointers

```rust
unsafe fn bad() -> *const i32 {
    let x = 42;
    &x as *const i32  // BAD: rets addr of stack value
}

unsafe fn good() -> Box<i32> {
    Box::new(42)  // GOOD: heap alloc survives past return
}
```

### Null pointer dereference

```rust
unsafe fn check_null(ptr: *const i32) -> i32 {
    if ptr.is_null() {
        return 0;
    }
    *ptr  // safe once nil guard passes
}
```

### Use after free

```rust
unsafe fn bad() {
    let b = Box::new(42);
    let ptr = &*b as *const i32;
    drop(b);  // BAD: frees mem
    let x = *ptr;  // BAD: ptr now dangles
}

unsafe fn good() {
    let b = Box::new(42);
    let x = *b;  // GOOD: read before drop
    drop(b);
}
```

## Philosophy

Cpprust equals C++-style RAII plus Rust syntax minus Rust safety nets.

- `unsafe` marks remain explicit everywhere, echoing C++ expectations.
- RAII scopes handle cleanup automatically, as in modern C++.
- Public interfaces lean on raw pointers and borrowed views.
- Borrow checker stays out of public contracts, placing responsibility on callers.
- Manual reasoning about lifetime and aliasing stays mandatory.

### When Cpprust shines

- Performance-sensitive systems or engine code.
- Interop layers touching C or C++ libraries.
- Layout-critical components where control matters more than safety.
- Teams willing to trade compile-time checking for predictable patterns.

### When Cpprust falls short

- General application development that can rely on safe Rust.
- Highly concurrent code better served by safe synchronisation primitives.
- Public crates that must feel idiomatic to the wider Rust ecosystem.
