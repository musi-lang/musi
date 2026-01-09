# Rust-Specific Rules

## Edition and Lints

This project uses **Rust 2024 edition** with strict linting enabled. All code must compile without warnings under these settings.

## Forbidden Patterns

The following are **compilation errors** in this project, not suggestions:

### Absolutely Forbidden (will not compile)

- `unsafe` code in any form
- `keyword_idents_2024` violations
- Missing `unsafe` annotations on extern blocks
- Unused `Result` or `#[must_use]` return values

### Denied by Clippy (will fail CI)

- `.unwrap()` in production code (use `.expect("reason")` instead)
- `.expect()` in test code (use `.unwrap()` instead)
- `panic!()`, `unimplemented!()`, `unreachable!()`, `todo!()`
- `dbg!()` macro
- String indexing/slicing (`str[..]`) - use `.chars()` or byte operations
- `as` casts - use explicit conversion methods
- Underscore type conversions (`as _`)
- `.get().unwrap()` - use pattern matching or `.get().expect()`
- Absolute paths in imports
- `Arc<Mutex<_>>` - use `Arc<RwLock<_>>` or atomic types
- `std::process::exit()` (commented as allowed for CLI tools)

## What This Means for Code Generation

### Error Handling

**Never generate:**

```rust
let value = result.unwrap();
let item = vec[index];
```

**Always generate:**

```rust
// In production code:
let value = result.expect("configuration must be valid");
let item = vec.get(index).expect("index validated above");

// In test code:
let value = result.unwrap();
```

### Type Conversions

**Never generate:**

```rust
let x = value as u32;
let y = ptr as usize;
```

**Always generate:**

```rust
let x = u32::try_from(value).expect("value fits in u32");
let y = usize::try_from(ptr).expect("pointer conversion valid");
// or use checked conversions when overflow matters
```

### String Operations

**Never generate:**

```rust
let c = s[0];
let slice = s[1..3];
```

**Always generate:**

```rust
let c = s.chars().next().expect("string is non-empty");
let slice = s.get(1..3).expect("valid UTF-8 range");
```

### Temporary Debugging

**Never generate:**

```rust
dbg!(value);
println!("{:?}", value);  // for debugging
```

**Always generate:**

```rust
// Use proper logging or tracing
tracing::debug!(?value, "checking value");
// Or ask user what logging approach to use
```

## Loop Patterns

The lint `infinite_loop` is denied. This means:

**Never generate:**

```rust
loop {
    // code with no break
}
```

**Always generate:**

```rust
// Ensure every `loop` has a clear exit condition
loop {
    if condition {
        break;
    }
    // work
}

// Or use while/for when possible
while condition {
    // work
}
```

## Documentation Requirements

While documentation lints aren't explicitly listed, assume:

- Public APIs need doc comments
- Safety comments are not needed (unsafe is forbidden)
- Don't add safety docs where there's no unsafe code

## Imports and Dependencies

- Use relative imports, not absolute paths
- Check existing import style in the module
- Don't add external dependencies without asking

## Testing Considerations

The lint `tests_outside_test_module` is denied.

**Structure tests as:**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_something() {
        // test code here
        let result = do_thing().unwrap();  // .unwrap() OK in tests
    }
}
```

## Common Clippy Categories

All these are set to "deny":

- `cargo` - dependency and manifest issues
- `complexity` - unnecessarily complex code
- `correctness` - likely bugs
- `nursery` - experimental lints (enabled here)
- `pedantic` - stylistic strictness
- `perf` - performance issues
- `style` - idiomatic style
- `suspicious` - suspicious patterns

Assume all lints in these categories will cause compilation failure.

## When Suggesting Changes

If you need to suggest code that would violate these lints:

1. State clearly: "This would violate the [lint_name] lint"
2. Provide the compliant alternative
3. Don't argue that the lint is too strict

The lints are project policy, not negotiable.

## Checking Your Code

Before generating Rust code, verify:

- [ ] No `unsafe` keyword anywhere
- [ ] No `.unwrap()` in non-test code
- [ ] No `panic!()`, `todo!()`, `unimplemented!()`, `unreachable!()`
- [ ] No `as` casts (use `try_from`/`try_into`)
- [ ] No string indexing (use `.chars()` or `.get()`)
- [ ] No `dbg!()` macro
- [ ] All `Result`/`must_use` values are used
- [ ] All loops have clear exit conditions
- [ ] Tests are inside `#[cfg(test)] mod tests`

If you generate code that violates these, it will fail to compile. Ask instead of guessing.
