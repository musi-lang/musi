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

- [ ] Define whether block comments can nest; if not, state it normatively
- [ ] Define whether rune/string literals may contain raw newlines normatively (current lexer allows it)
- [ ] Define whether template literals may contain raw newlines normatively

## Template Literals

- [ ] Decide whether template literals support `${ expr }` interpolation in the core syntax
- [ ] If yes: define brace escaping (`{{` / `}}`?) and nesting rules
- [ ] Specify how template interpolation is tokenized and parsed (current ANTLR grammar treats it as one literal token)

## Symbolic operators

- [ ] Specify “precedence by family” rule:
  - [ ] How a family is derived from an operator string (first char? full string? partitioning?)
  - [ ] Whether mixed-symbol operators are allowed (`==<<==>>==`)
  - [ ] Associativity per family (or global default)
  - [ ] How collisions with reserved compound tokens are rejected (normative rule)
- [ ] Update `grammar/Musi.g4` to reflect the finalized rule (or explicitly document as semantic rule outside the grammar)

## Syntactic inconsistencies to resolve

- [ ] Fix/confirm `let` binder ordering: `name [T] (params)` vs `name (params) [T]` (ANTLR grammar currently uses `[T]` then `(params)`)
- [ ] Confirm record literal vs record update spellings (`{ ... }` vs `expr.{ ... }`)
- [ ] Decide whether postfix `...` exists (binding-power table mentions it; productions do not)

## Semantic follow-ups after grammar unification

- [ ] Define semantic recognition rules for built-in universes (`Type`, `Type0`, `Type1`, ...)
- [ ] Define semantic validity rules for `import expr` operands
