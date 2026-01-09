# Rust-Specific Rules

<language_profile name="rust" edition="2024">

<strict_compilation_rules>
The following are **HARD ERRORS**. Code containing these will be rejected:

1. **`unsafe`**: STRICTLY FORBIDDEN unless explicitly requested by user.
2. **`.unwrap()`**: FORBIDDEN in `src/`. Allowed ONLY in `tests/`. Use `.expect()` or `?`.
3. **`panic!()` / `todo!()`**: FORBIDDEN in production code.
4. **`as` Casting**: FORBIDDEN. Use `try_from` or `from` for type safety.
</strict_compilation_rules>

<anti_patterns>

## Common Hallucinations to Avoid

* **Dependency Hallucination**: Do NOT use crates (e.g., `anyhow`, `thiserror`, `tokio`) unless they are ALREADY in `Cargo.toml`.
* **Macro Hallucination**: Do not invent macros. Check imports.
* **Lazy Error Handling**:
  * ❌ `let _ = func();` (Silencing errors)
  * ❌ `Box<dyn Error>` (unless currently used in file)

## Correctness Check

* **Ownership**: Ensure no partial moves.
* **Async**: If functions are `async`, ensure `.await` is used.
* **Loops**: All `loop {}` must have a `break` condition reachable in finite time.
</anti_patterns>

<testing_standards>

* **Unit Tests**: Must be in `#[cfg(test)] mod tests` at the bottom of the file, split into `module/tests.rs`.
* **No Mocks**: Prefer integration style testing over heavy mocking unless existing pattern dictates.
</testing_standards>

</language_profile>
