# Phase 2 -- Lexer

**Crate:** `musi_lex`
**Goal:** Complete tokenizer covering every lexical production in the grammar.
**Dependencies:** Phase 1 (musi_shared)

---

## Deliverables

### TokenKind Enum

Every distinct token the grammar requires.

**Naming convention:** punctuation tokens are named after their symbol shape, not their semantic role
(e.g. `..` → `DotDot`, `=` → `Eq`, `!` → `Bang`). Keyword tokens use the keyword word as PascalCase.

**Keywords (31, from `lex_keyword` in grammar.ebnf):**
`Fn`, `Const`, `Var`, `If`, `Then`, `Elif`, `Else`, `Match`, `Case`, `While`, `Loop`, `For`, `In`,
`Break`, `Cycle`, `Return`, `Defer`, `Import`, `From`, `Export`, `Native`, `Opaque`, `Record`,
`Choice`, `And`, `Or`, `Xor`, `Not`, `As`, `With`, `Label`

> **Note:** `shl` and `shr` appear as infix operators in `ast_expr_shift` but are absent from
> `lex_keyword`. They must be treated as keywords (otherwise `let shl = 5;` would be valid).
> Grammar needs a fix; the lexer will treat them as keywords (`Shl`, `Shr`) -- total 33 keywords.

**Punctuation / Operators:**

| Token text | Variant name |
| ---------- | ------------ |
| `(`        | `LParen`     |
| `)`        | `RParen`     |
| `{`        | `LBrace`     |
| `}`        | `RBrace`     |
| `[`        | `LBracket`   |
| `]`        | `RBracket`   |
| `,`        | `Comma`      |
| `;`        | `Semi`       |
| `:`        | `Colon`      |
| `::`       | `ColonColon` |
| `.`        | `Dot`        |
| `..`       | `DotDot`     |
| `..<`      | `DotDotLt`   |
| `->`       | `MinusGt`    |
| `=>`       | `EqGt`       |
| `<-`       | `LtMinus`    |
| `:=`       | `ColonEq`    |
| `=`        | `Eq`         |
| `/=`       | `SlashEq`    |
| `+`        | `Plus`       |
| `-`        | `Minus`      |
| `*`        | `Star`       |
| `/`        | `Slash`      |
| `%`        | `Percent`    |
| `<`        | `Lt`         |
| `>`        | `Gt`         |
| `<=`       | `LtEq`       |
| `>=`       | `GtEq`       |
| `\|`       | `Pipe`       |
| `&`        | `Amp`        |
| `^`        | `Caret`      |
| `~`        | `Tilde`      |
| `!`        | `Bang`       |
| `@`        | `At`         |
| `#`        | `Hash`       |
| `_`        | `Underscore` |

**Compound tokens (3):**

| Token text | Variant name  | Meaning           |
| ---------- | ------------- | ----------------- |
| `.[`       | `DotLBracket` | index access      |
| `.{`       | `DotLBrace`   | record dot update |
| `<..`      | `LtDotDot`    | spread operator   |

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
    '`'                          → lex_escaped_ident()
    '\''                         → lex_ty_ident_or_char()
    '0'..='9'                    → lex_number()
    '"'                          → lex_string()
    '/' if peek(1) == '/'        → lex_or_skip_comment(); next_token()
    '.' if peek(1) == '['        → emit(DotLBracket, 2)
    '.' if peek(1) == '{'        → emit(DotLBrace, 2)
    '.' if peek(1) == '.'        →
       if peek(2) == '<' → emit(DotDotLt, 3)
       else              → emit(DotDot, 2)
    '<' if peek(1) == '.' and peek(2) == '.' → emit(LtDotDot, 3)
    _                            → lex_punctuation()
```

**Keyword resolution:** Lex identifier text, look up in pre-interned keyword table.
If match → keyword `TokenKind`, else → `Ident`.

**Compound token disambiguation:**
- `.` peeks next: `[` → `DotLBracket`, `{` → `DotLBrace`, `.` → see dot-dot branch, else → `Dot`
- `..` peeks next: `<` → `DotDotLt`, else → `DotDot`
- `<` peeks next two: `..` → `LtDotDot`, `=` → `LtEq`, `-` → `LtMinus`, else → `Lt`
- `:` peeks next: `:` → `ColonColon`, `=` → `ColonEq`, else → `Colon`
- `/` peeks next: `=` → `SlashEq`, `/` → comment, else → `Slash`
- `-` peeks next: `>` → `MinusGt`, else → `Minus`
- `=` peeks next: `>` → `EqGt`, else → `Eq`

**`'a` vs `'x'` disambiguation:**
- `'` followed by letter then `'` → char literal (`'x'`)
- `'` followed by letter (no closing `'`) → type ident (`'a`)

**Number lexing:**
- `0x` → hex (`xdigit+`), `0o` → octal (`odigit+`), `0b` → binary (`bdigit+`)
- Decimal with optional `.digit+` fraction → float
- `_` separators allowed anywhere in a numeric literal

**String lexing:**
- Standard escape sequences: `\\`, `\"`, `\n`, `\t`, `\r`, `\0`, `\x{HH}`, `\u{HHHH}`
- Unterminated string → `Error` token + diagnostic

**Comment handling:**
- `//` line comment → skip to end of line
- `///` line doc comment → emit `DocComment` token with interned text
- `/*` block comment → skip (may nest? -- grammar says `{char}` which is non-nesting; treat as non-nesting)

**Error recovery:** Invalid character → emit `Error` token with single-char span, advance one byte,
continue lexing.

---

## Milestone

1. Lex `examples/hello.ms` into correct token sequence.
2. Lex every keyword → correct `TokenKind`.
3. Lex all numeric bases (`0xFF`, `0o77`, `0b1010`, `42`, `3.14`).
4. Lex compound tokens (`DotLBracket`, `DotLBrace`, `LtDotDot`, `DotDotLt`).
5. Lex type idents (`'a`, `'key`) distinctly from char literals (`'x'`).
6. Lex escaped idents (`` `some-ident` ``).
7. Lex `=` (equality) and `/=` (inequality) correctly (not `==` / `!=`).
8. Invalid input → `Error` token, lexer continues.
9. `cargo test -p musi_lex` passes.
