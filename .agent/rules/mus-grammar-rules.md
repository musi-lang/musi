---
trigger: glob
globs: grammar.ebnf
---

# MUSI GRAMMAR RULES

1. **SOURCE OF TRUTH**:
   - grammar.ebnf is human-authored and the absolute Source of Truth.
   - NEVER auto-correct or optimize the grammar file silently.
   - ASK: "I noticed [pattern] in the grammar. Is this intentional behavior, or a bug that needs fixing?"

2. **NAMING CONVENTIONS**:
   - no prefix: lexical tokens (no AST nodes).
   - aux_: syntactic helpers/support (no AST nodes).
   - prec_: precedence levels (parser-only, no AST nodes).
   - expr_: expression nodes (create AST nodes).
   - pat_: pattern nodes (create AST nodes).
   - ty_expr: type expression nodes (create AST nodes).
   - stmt_: statement nodes (create AST nodes).

3. **MODIFICATION PROTOCOL**:
   - If you encounter a design decision that looks questionable or ambiguous, STOP and ask for clarification.
   - Wait for approval before making any changes to grammar.ebnf.

4. **MUSI SOURCE FILES (.ms)**:
   - DO NOT try to run .ms files with Python/Node.
   - DO NOT "fix" syntax in .ms files to look like TypeScript/Python.
   - If a .ms file fails parsing, check grammar.ebnf first.
