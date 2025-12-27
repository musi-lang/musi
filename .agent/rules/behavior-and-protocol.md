---
trigger: always_on
---

# AGENT BEHAVIOR & PROTOCOL

1. **ZERO-TRUST FILE OPERATIONS**:
   - You are FORBIDDEN from completely wiping/rewriting a file unless the user prompt explicitly says "rewrite file".
   - **MANDATORY**: Before applying changes to any file >100 lines, you must generate a diff preview in the chat and ASK for confirmation.
   - NEVER inline imports or remove "unused" code without asking.

2. **NO ASSUMPTIONS**:
   - If a prompt is ambiguous (e.g., "fix this"), ASK clarifying questions.
   - Do not assume the user wants to refactor the whole file. Fix ONLY what is broken.
   - **MANDATORY**: Stop if you are guessing intent. Avoid being assumptious.

3. **ARTIFACT-FIRST WORKFLOW**:
   - For any task involving >3 files OR multiple crates, you must first generate a Plan Artifact (markdown list) of what you intend to do. (Standard >2 files for small single-crate tasks).
   - Wait for user approval before executing the plan.

4. **ANTI-PATTERNS & FORBIDDEN CODE**:
   - **BANNED**: TODO, "for now", "simplified for now", "placeholder", or any workaround markers.
   - **BANNED**: Hacky solutions or "disgusting" placeholders.
   - **MANDATORY**: Adhere strictly to DRY, SRP, and KISS principles.
   - **MANDATORY**: No code comments unless explicitly asked by the user.

5. **PRIORITY**:
   - **Lint Compliance** (clippy/cargo) takes priority over all other rules. Do NOT suppress lints unless unavoidable (see 01-rust.md).
