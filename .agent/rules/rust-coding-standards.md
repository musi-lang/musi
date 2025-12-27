---
trigger: glob
globs: **/*.rs
---

# RUST CODING STANDARDS

1. **COMPLEXITY GATES**:
   - **Hard Limit**: Functions/Methods MUST be under 50 lines.
   - If a function exceeds this, you MUST refactor it into atomic sub-functions immediately.
   - **Nesting**: Max 3 levels deep. Use Guard Clauses (return early) to flatten logic.

2. **DRY ENFORCEMENT**:
   - Duplicate logic is a critical error. Extract shared logic to private helper functions.
   - Do not copy-paste blocks of code to "save time".

3. **NAMING CONVENTIONS**:
   - Respect Existing Style: Scan the file. If snake_case, use it. If camelCase, use it.
   - **Variable Naming**:
     - Scope-level variables must be descriptive (userPayload, inventoryList).
     - BANNED: data, obj, item, result (unless inside a localized loop/lambda).
     - ALLOWED: i, ctx, req, err (standard idioms).

4. **ERROR HANDLING**:
   - .unwrap() is FORBIDDEN outside test files (tests.rs or @/tests/ dir).
   - Use .expect("msg") for inevitable panics (e.g. arena overflow).
   - Use Result<T, E> for all recoverable errors.

5. **FUNCTION ORDERING (impl blocks)**:
   1. Constructors (new, Default, From).
   2. Main Public Entry Points.
   3. High-level orchestrators.
   4. Specialized implementation methods.
   5. Internal private helpers.
   6. Low-level navigation (peek, advance).
   7. Character predicates at the bottom.

6. **BORROW CHECKER & CLOSURES**:
   - Avoid map_or_else/and_then closures capturing &mut self if already partially borrowed.
   - Prefer match or if let blocks for clearer lifetime boundaries.

7. **TESTING**:
   - Tests are written AFTER implementation.
   - Unit tests go in sibling foo/tests.rs files.
   - Integration tests go in /tests/ directory.
   - NEVER inline tests via mod tests { ... } in the same file.

8. **DOCUMENTATION**:
   - Doc comments (///) only for public API or when clippy suggests.
   - Private methods do not require documentation.
   - No code comments (//) unless explicitly asked.

**Reference**: See GEMINI.md for Arena Allocation and Pratt Parsing implementation details.
