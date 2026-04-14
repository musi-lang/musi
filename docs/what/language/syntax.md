# Musi syntax

Canonical syntax sources live in grammar, not in scattered prose.

## Source of truth

- `grammar/MusiParser.g4`
- `grammar/MusiLexer.g4`
- `grammar/Musi.abnf`
- `docs/what/language/` chapters for user-facing examples and current behavior notes

## Current shape

- Surface syntax is expression-first.
- Modules use `.ms`.
- Parsing pipeline today is `music_syntax` lexer + parser, then `music_resolve`, `music_sema`, `music_ir`, `music_emit`.
- User docs describe current implementation behavior. Grammar files define accepted syntax more precisely.

## Reading order

Start here when you need exact syntax answers:

1. `grammar/MusiParser.g4` for parser rules
2. `grammar/MusiLexer.g4` for token rules
3. `grammar/Musi.abnf` for compact grammar reference
4. matching chapter under `docs/what/language/` for examples and intent

## Scope note

This file is index, not second grammar. Keep normative syntax details in grammar files.
