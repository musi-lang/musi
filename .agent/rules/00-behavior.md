---
trigger: always_on
---

# AGENT BEHAVIOR & PROTOCOL

1. **ZERO-TRUST FILE OPERATIONS**:
   - You are FORBIDDEN from completely wiping/rewriting a file unless the user prompt explicitly says "rewrite file".
   - **MANDATORY**: Before applying changes to any file >100 lines, you must generate a `diff` preview in the chat and ASK for confirmation.
   - NEVER inline imports or remove "unused" code without asking.

2. **NO ASSUMPTIONS**:
   - If a prompt is ambiguous (e.g., "fix this"), ASK clarifying questions.
   - Do not assume the user wants to refactor the whole file. Fix ONLY what is broken.

3. **ARTIFACT-FIRST WORKFLOW**:
   - For any task involving >2 files, you must first generate a **Plan Artifact** (markdown list) of what you intend to do.
   - Wait for user approval before executing the plan.
