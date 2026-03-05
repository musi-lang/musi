# Phase 2 — Lexer

**Crate:** `musi_lex`
**Goal:** Complete tokenizer covering every lexical production in the grammar.
**Dependencies:** Phase 1 (musi_shared)

---

## Deliverables

### TokenKind Enum

Every distinct token the grammar requires:

**Keywords (32):**
`fn`, `const`, `var`, `if`, `elif`, `else`, `match`, `case`, `while`, `loop`, `for`, `in`, `break`, `cycle`, `return`, `defer`, `import`, `from`, `export`, `native`, `record`, `choice`, `type`, `and`, `or`, `not`, `is`, `as`, `mod`, `pub`, `label`, `mut`

**Punctuation/Operators (~25):**
`(`, `)`, `{`, `}`, `[`, `]`, `,`, `;`, `:`, `::`, `.`, `..`, `->`, `=>`, `<-`, `:=`, `=`, `+`, `-`, `*`, `/`, `%`, `<`, `>`, `<=`, `>=`, `==`, `!=`, `|`, `&`, `^`, `~`, `!`, `@`, `#`, `_`

**Compound tokens (3):**
`.[` (index access), `.{` (record literal), `<..` (spread)

**Literals (4 kinds):**
`IntLit`, `FloatLit`, `StringLit`, `CharLit`

**Other:**
`Ident`, `TyIdent` (starts with `'`), `DocComment` (`///`), `Eof`, `Error`

### Token Struct

```
Token = {
  kind: TokenKind,
  span: Span,
  symbol: Option<Symbol>,  // for Ident, TyIdent, literals, doc comments
}
```

### Lexer

Hand-written, single-pass, `Iterator<Item = Token>`.

**Algorithm (pseudo-code):**
```
fn next_token():
  skip_whitespace_and_comments()
  if at_end: return Token(Eof, current_span())

  ch = peek()
  match ch:
    'a'..='z' | 'A'..='Z' | '_' → lex_ident_or_keyword()
    '\'' → lex_ty_ident_or_char()
    '0'..='9' → lex_number()
    '"' → lex_string()
    '/' if peek(1) == '/' → skip_line_comment(); next_token()  // /// = doc comment
    '.' if peek(1) == '[' → emit(DotBracket, 2)
    '.' if peek(1) == '{' → emit(DotBrace, 2)
    '<' if peek(1) == '.' and peek(2) == '.' → emit(Spread, 3)
    _ → lex_punctuation()
```

**Keyword resolution:** Lex identifier text, then look up in keyword table (pre-interned symbols). If match → keyword `TokenKind`, else → `Ident`.

**Compound token disambiguation:**
- `.` peeks next char: `[` → `.[`, `{` → `.{`, `.` → `..`, else → `.`
- `<` peeks next two: `..` → `<..`, `=` → `<=`, `-` → `<-`, else → `<`

**`'a` vs `'x'` disambiguation:**
- `'` followed by letter then `'` → char literal (`'x'`)
- `'` followed by letter (no closing `'`) → type ident (`'a`)

**Number lexing:**
- `0x` → hex, `0o` → octal, `0b` → binary
- Decimal with optional `.` fraction → float
- `_` separators allowed in numeric literals

**String lexing:**
- Standard escape sequences: `\\`, `\"`, `\n`, `\t`, `\r`, `\0`, `\x{HH}`, `\u{HHHH}`
- Unterminated string → `Error` token + diagnostic

**Error recovery:** Invalid character → emit `Error` token with single-char span, advance one byte, continue lexing.

---

## Milestone

1. Lex `examples/hello.ms` into correct token sequence.
2. Lex every keyword → correct `TokenKind`.
3. Lex all numeric bases (`0xFF`, `0o77`, `0b1010`, `42`, `3.14`).
4. Lex compound tokens (`.[`, `.{`, `<..`).
5. Lex type idents (`'a`, `'key`) distinctly from char literals (`'x'`).
6. Lex escaped idents (`` `some-ident` ``).
7. Invalid input → `Error` token, lexer continues.
8. `cargo test -p musi_lex` passes.
