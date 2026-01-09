# Code Style and Formatting

<coding_standards>

## Context Engineering

* **Match Pattern**: Do not introduce new patterns (e.g., new error handling libraries, new folder structures) unless explicitly asked.
* **Mimicry**: Code you write must be indistinguishable from the existing codebase.

## Comments & Documentation

* **Inline Comments**: **FORBIDDEN** for obvious logic. **REQUIRED** for:
  * Unsafe blocks (if allowed).
  * Complex algorithms (Time complexity > O(n)).
  * Workarounds for external bugs.
* **Doc Comments**: Public APIs must have documentation matching the language standard (e.g., Rust `///`, Python `"""`).

</coding_standards>

<formatting_rules>

* **Strict Adherence**: Follow the project's linter/formatter config (`.rustfmt.toml`, `.eslintrc`, etc.) exactly.
* **No Reformatting**: Do not reformat unrelated code. It creates noise in the diff.
* **Imports**: Group imports logically. Remove unused imports immediately.
</formatting_rules>

<completeness_enforcement>

## "Definition of Done"

Every code block provided must be:

1. **Syntactically Correct**: No missing brackets or typos.
2. **Logically Complete**: All edge cases (nulls, empty lists, errors) handled.
3. **Dependency Checked**: No usage of functions/types that are not imported or defined.

### ❌ REJECTED PATTERN (Lazy)

```python
def process_data(data):
    # TODO: implement validation
    pass
```

### ✅ ACCEPTED PATTERN (Complete)

```python
def process_data(data):
    if not data:
        raise ValueError("data cannot be empty")
    validate_schema(data) # assumes validate_schema exists in context
    return _transform(data)
```

</completeness_enforcement>
