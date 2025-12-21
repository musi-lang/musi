---
trigger: glob
globs: **/*.{c,cc,cpp,css,cxx,go,h,hh,hpp,html,hxx,js,jsx,mjs,ml,mli,mts,ms,py,rb,rs,scala,sc,scss,ts,tsx}
---

# STRICT CODING STANDARDS

1. **COMPLEXITY GATES**:
   - **Hard Limit**: Functions/Methods MUST be under 50 lines.
   - If a function exceeds this, you MUST refactor it into atomic sub-functions immediately.
   - **Nesting**: Max 3 levels deep. Use Guard Clauses (`return early`) to flatten logic.

2. **DRY ENFORCEMENT**:
   - Duplicate logic is a critical error. Extract shared logic to private helper functions.
   - Do not copy-paste blocks of code to "save time".

3. **NAMING CONVENTIONS**:
   - **Respect Existing Style**: Scan the file. If `snake_case`, use it. If `camelCase`, use it.
   - **Variable Naming**:
     - Scope-level variables must be descriptive (`userPayload`, `inventoryList`).
     - BANNED: `data`, `obj`, `item`, `result` (unless inside a localized loop/lambda).
     - ALLOWED: `i`, `ctx`, `req`, `err` (standard idioms).

4. **ERROR HANDLING**:
   - NEVER swallow errors with empty catch blocks.
   - Maintain existing error message formats.
