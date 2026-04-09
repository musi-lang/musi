# ANTLR Grammar Tracker

Canonical, tool-supported grammar: `grammar/Musi.g4`.

This tracker is for the remaining semantic clarifications needed to make the grammar fully normative (not just parseable).

## Status

- [x] Add initial ANTLR4 grammar file (`grammar/Musi.g4`)
- [x] Make `grammar/Musi.abnf` a strict RFC 5234 ABNF specification (aligned with `grammar/Musi.g4`)
- [x] Remove the separate `type_expr` grammar category in favor of one expression space
- [x] Treat `Type`, `Type0`, `Type1`, ... as ordinary identifiers in the lexer/parser
- [x] Promote `instance` to an ordinary expression form
- [x] Add generic bracket application `expr[...]`

## Lexical contracts

- [x] Block comments do not nest (`/* ... */`, `/** ... */`)
- [x] String/rune literals do not contain raw newlines (no raw `\n` or `\r`)
- [x] Template literals may contain raw newlines

## Template Literals

- [x] Decide whether template literals support `${ expr }` interpolation in the core syntax
- [x] Define nesting rule (interpolation ends at `}` when brace depth returns to 0)
- [x] Specify how template interpolation is tokenized and parsed (lexer modes + `TEMPLATE_BEGIN`/`TEMPLATE_TEXT`/`TEMPLATE_INTERP_BEGIN`/`TEMPLATE_END`)

## Symbolic operators

- [x] Define fixity declarations (`infixl`/`infixr`/`infix`) for symbolic operators
- [x] Parse infix operators as a flat chain; resolve precedence/associativity semantically

## Syntactic inconsistencies to resolve

- [ ] Fix/confirm `let` binder ordering: `name [T] (params)` vs `name (params) [T]` (ANTLR grammar currently uses `[T]` then `(params)`)
- [ ] Confirm record literal vs record update spellings (`{ ... }` vs `expr.{ ... }`)
- [ ] Decide whether postfix `...` exists (binding-power table mentions it; productions do not)

## Semantic follow-ups after grammar unification

- [ ] Define semantic recognition rules for built-in universes (`Type`, `Type0`, `Type1`, ...)
- [ ] Define semantic validity rules for `import expr` operands
