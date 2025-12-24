# Parser Implementation Plan

Pratt parsing for expressions, recursive descent for types/patterns/statements.

---

## File Structure

```text
musi_parse/src/
├── lib.rs          # Public API
├── parser.rs       # Parser core, token stream, helpers
├── expr.rs         # Pratt expression parser
├── typ.rs          # Type parser (rec-desc)
├── pat.rs          # Pattern parser (rec-desc)
├── stmt.rs         # Statement parser
└── error.rs        # ParseErrorKind (follows lexer pattern)
```

---

## Parser Helpers (`parser.rs`)

```rust
impl Parser<'_> {
    fn peek(&self) -> Option<TokenKind>;
    fn peek_nth(&self, n: usize) -> Option<TokenKind>;
    fn advance(&mut self) -> Option<&Token>;
    fn advance_by(&mut self, n: usize);  // for `not in`, etc.
    fn at(&self, kind: TokenKind) -> bool;
    fn eat(&mut self, kind: TokenKind) -> bool;
    fn expect(&mut self, kind: TokenKind) -> Result<&Token>;

    // Sync helpers
    fn sync(&mut self);                    // sync to `;` or `}`
    fn sync_to_stmt(&mut self);            // sync to next statement
    fn sync_to_block_end(&mut self);       // sync to `}`
}
```

---

## Pratt Precedence Table

| Level | Operators | Assoc |
|-------|-----------|-------|
| 16 | `<-` | Right |
| 15 | `|>` | Left |
| 14 | `??` | Left |
| 13 | `KwOr` | Left |
| 12 | `KwAnd` | Left |
| 11 | `Pipe` | Left |
| 10 | `Caret` | Left |
| 9 | `Amp` | Left |
| 8 | `Eq` `SlashEq` | Non |
| 7 | `Lt` `Gt` `LtEq` `GtEq` `KwIs` `KwAs` `KwIn` | Non |
| 6 | `DotDot` `DotDotLt` | Non |
| 5 | `ColonColon` | Right |
| 4 | `Plus` `Minus` | Left |
| 3 | `Star` `Slash` `Percent` `KwMod` `LtLt` `GtGt` | Left |
| 2 | `StarStar` | Right |
| 1 | Prefix: `Minus` `KwNot` `Tilde` `At` | — |
| 0 | Postfix: `()` `[]` `.ident` `.^` `?` | — |

### `not in` Handling

```rust
// At relational precedence level:
if self.at(KwNot) && self.peek_nth(1) == Some(KwIn) {
    self.advance_by(2); // eat `not in`
    let rhs = self.parse_expr_bp(rel_bp);
    lhs = Unary { op: KwNot, operand: Binary { op: KwIn, lhs, rhs } };
}
```

---

## Syntactic Errors (`error.rs`)

Follows lexer pattern — hints via `IntoMusiError::hint()`:

```rust
#[derive(Debug, Clone, Error)]
#[non_exhaustive]
pub enum ParseErrorKind {
    #[error("expected {0}")]
    Expected(&'static str),
    #[error("expected {0} after {1}")]
    ExpectedAfter(&'static str, &'static str),
    #[error("unexpected {0}")]
    Unexpected(String),
    #[error("unexpected end of file")]
    UnexpectedEof,
    #[error("unclosed {0}")]
    Unclosed(&'static str),
    #[error("invalid {0}")]
    Invalid(&'static str),
}
```

---

## Remaining Tasks

- [ ] Create `expr.rs` with Pratt expression parser
- [ ] Create `typ.rs` with type parser
- [ ] Create `pat.rs` with pattern parser
- [ ] Create `stmt.rs` with statement parser
- [ ] Integrate with `lib.rs` public API
- [ ] Verify with unit tests in tests.rs, following similar pattern to `crates/musi-lex/src/tests.rs`
