# Phase 3 — Parser + AST

**Crate:** `musi_parse`
**Goal:** Complete LL(1) + Pratt parser producing a full AST, plus all AST type definitions.
**Dependencies:** Phase 1 (musi_shared), Phase 2 (musi_lex)

---

## Deliverables

### File layout

```
crates/musi_parse/src/
  lib.rs
  ast.rs      — all AST type definitions + ParseCtx + ParsedModule
  parser.rs   — Parser struct, all parse methods (LL(1) + Pratt)
  sexpr.rs    — deterministic S-expression printer for AST debugging
```

---

## AST Types (`ast.rs`)

All node types contain a `span: Span` field. Recursive single children use `Idx<T>` (arena-allocated). List children use `Vec<Idx<T>>` or `Vec<T>` for leaf-like items (Ty, Pat, etc. that are small).

### Arena container

```rust
pub struct ParseCtx {
    pub exprs: Arena<Expr>,
    pub tys:   Arena<Ty>,
    pub pats:  Arena<Pat>,
}

pub struct ParsedModule {
    pub items: Vec<Idx<Expr>>,   // top-level statement expressions
    pub ctx:   ParseCtx,
    pub span:  Span,
}
```

### Supporting leaf types

```rust
pub enum LitValue {
    Int(i64),
    Float(f64),
    Str(Symbol),
    Char(char),
    Bool(bool),
}

pub enum BindKind { Const, Var }

pub enum Modifier {
    Export,
    Opaque,
    Native(Option<Symbol>),   // native ["abi-string"]
}

pub struct Attr {
    pub name:   Symbol,
    pub args:   Vec<AttrArg>,
    pub span:   Span,
}

pub enum AttrArg {
    Named { name: Symbol, value: Option<LitValue>, span: Span },
    Lit(LitValue, Span),
}

pub struct TyParam {
    pub name:   Symbol,    // 'a, 'key — the ident token text including the '
    pub bounds: Vec<Ty>,
    pub span:   Span,
}

pub struct Param {
    pub attrs:   Vec<Attr>,
    pub mutable: bool,
    pub name:    Symbol,   // _ is allowed (wildcard param)
    pub ty:      Option<Ty>,
    pub span:    Span,
}

pub struct RecField {
    pub attrs:   Vec<Attr>,
    pub mutable: bool,
    pub name:    Symbol,
    pub ty:      Option<Ty>,
    pub span:    Span,
}

pub struct ImportItem {
    pub name:  Symbol,
    pub alias: Option<Symbol>,
    pub span:  Span,
}

pub struct MatchArm {
    pub attrs: Vec<Attr>,
    pub pat:   Pat,
    pub guard: Option<Idx<Expr>>,
    pub body:  Idx<Expr>,
    pub span:  Span,
}

pub struct ChoiceVariant {
    pub attrs:   Vec<Attr>,
    pub name:    Symbol,
    pub payload: Option<VariantPayload>,
    pub span:    Span,
}

pub enum VariantPayload {
    Positional(Vec<Ty>),
    Named(Vec<RecField>),
}

/// A condition in `if`/`while`/`loop` — either a plain expression or a
/// pattern-binding destructure: `case const/var pat := expr`.
pub enum Cond {
    Expr(Idx<Expr>),
    Case {
        kind: BindKind,
        pat:  Pat,
        init: Idx<Expr>,
        span: Span,
    },
}
```

### BinOp (grammar-derived, NOT the plan's original table)

```rust
pub enum BinOp {
    // Arithmetic
    Add, Sub, Mul, Div, Rem,
    // Bitwise
    BitOr, BitXor, BitAnd, Shl, Shr,
    // Logical
    And, Or, Xor,
    // Comparison (non-associative in grammar)
    Eq, NotEq, Lt, Gt, LtEq, GtEq, In,
    // Range (non-associative)
    Range,      // ..
    RangeExcl,  // ..<
    // Cons (left-assoc)
    Cons,       // ::
}
```

### PrefixOp

```rust
pub enum PrefixOp {
    Neg,    // -
    Not,    // not
    Deref,  // !
    AddrOf, // @
    BitNot, // ~
}
```

### PostfixOp

```rust
pub enum PostfixOp {
    Call  { args: Vec<Idx<Expr>>, span: Span },
    Index { args: Vec<Idx<Expr>>, span: Span },         // .[ expr_list ]
    Field { name: Symbol, span: Span },                  // .name or .0
    RecDot { fields: Vec<RecLitField>, span: Span },    // .{ fields }
    As    { ty: Ty, span: Span },                        // as Type (postfix cast)
}

pub enum RecLitField {
    Named  { attrs: Vec<Attr>, mutable: bool, name: Symbol, value: Idx<Expr>, span: Span },
    Spread { expr: Idx<Expr>, span: Span },
}
```

### Ty

```rust
pub enum Ty {
    Arrow  { params: Vec<Ty>, ret: Box<Ty>, span: Span },
    Named  { name: Symbol, args: Vec<Ty>, span: Span },    // Int or Option['T]
    Prod   { elements: Vec<Ty>, span: Span },               // (A, B) or ()
    Arr    { element: Box<Ty>, size: Option<Idx<Expr>>, span: Span },
    Var    { name: Symbol, span: Span },                    // 'a
    Error  { span: Span },
}
```

### Pat

```rust
pub enum Pat {
    Ident  { name: Symbol, suffix: Option<PatSuffix>, span: Span },
    Lit    { value: LitValue, span: Span },
    Wild   { span: Span },                                  // _
    Prod   { elements: Vec<Pat>, span: Span },              // (p1, p2)
    Arr    { elements: Vec<Pat>, span: Span },              // [p1, p2]
    AnonRec { fields: Vec<PatField>, span: Span },          // { f: p }
    Or     { alternatives: Vec<Pat>, span: Span },
    Error  { span: Span },
}

pub enum PatSuffix {
    Positional { args: Vec<Pat>, span: Span },             // Name(p1, p2)
    Named      { fields: Vec<PatField>, span: Span },      // Name{ f: p }
}

pub struct PatField {
    pub attrs:   Vec<Attr>,
    pub mutable: bool,
    pub name:    Symbol,
    pub pat:     Option<Pat>,   // None = shorthand (field name is also binding)
    pub span:    Span,
}
```

### Expr

```rust
pub enum Expr {
    // Atoms
    Lit    { value: LitValue, span: Span },
    Ident  { name: Symbol, span: Span },
    Unit   { span: Span },                                  // ()
    Paren  { inner: Idx<Expr>, span: Span },               // (e)
    Tuple  { elements: Vec<Idx<Expr>>, span: Span },       // (e1, e2)
    Block  { stmts: Vec<Idx<Expr>>, tail: Option<Idx<Expr>>, span: Span },  // (s; s; e)
    Array  { items: Vec<ArrayItem>, span: Span },
    AnonRec { fields: Vec<RecLitField>, span: Span },       // { f := v }

    // Control flow
    If     { cond: Box<Cond>, then_body: Idx<Expr>,
             elif_chains: Vec<ElifChain>, else_body: Option<Idx<Expr>>, span: Span },
    Match  { scrutinee: Idx<Expr>, arms: Vec<MatchArm>, span: Span },
    While  { cond: Box<Cond>, guard: Option<Idx<Expr>>, body: Idx<Expr>, span: Span },
    Loop   { body: Idx<Expr>, post_cond: Option<Box<Cond>>, span: Span },
    For    { pat: Pat, iter: Idx<Expr>, guard: Option<Idx<Expr>>, body: Idx<Expr>, span: Span },
    Label  { name: Symbol, body: Idx<Expr>, span: Span },
    Return { value: Option<Idx<Expr>>, span: Span },
    Break  { label: Option<Symbol>, value: Option<Idx<Expr>>, span: Span },
    Cycle  { label: Option<Symbol>, guard: Option<Idx<Expr>>, span: Span },
    Defer  { body: Idx<Expr>, span: Span },
    Import { items: ImportClause, path: Symbol, span: Span },

    // Declarations (first-class expressions in Musi)
    Record { attrs: Vec<Attr>, modifiers: Vec<Modifier>,
             name: Option<Symbol>, ty_params: Vec<TyParam>,
             fields: Vec<RecField>, span: Span },
    Choice { attrs: Vec<Attr>, modifiers: Vec<Modifier>,
             name: Option<Symbol>, ty_params: Vec<TyParam>,
             variants: Vec<ChoiceVariant>, span: Span },
    FnDef  { attrs: Vec<Attr>, modifiers: Vec<Modifier>,
             name: Symbol, ty_params: Vec<TyParam>,
             params: Vec<Param>, ret_ty: Option<Ty>,
             body: Option<Idx<Expr>>,    // None = native fn (no body)
             span: Span },
    Lambda { attrs: Vec<Attr>,
             ty_params: Vec<TyParam>, params: Vec<Param>,
             ret_ty: Option<Ty>, body: Idx<Expr>, span: Span },
    Bind   { attrs: Vec<Attr>, modifiers: Vec<Modifier>,
             kind: BindKind, pat: Pat,
             ty: Option<Ty>, init: Option<Idx<Expr>>, span: Span },

    // Operators
    Prefix  { op: PrefixOp, operand: Idx<Expr>, span: Span },
    Binary  { op: BinOp, lhs: Idx<Expr>, rhs: Idx<Expr>, span: Span },
    Assign  { target: Idx<Expr>, value: Idx<Expr>, span: Span },   // <-
    Postfix { base: Idx<Expr>, op: PostfixOp, span: Span },

    // Error sentinel
    Error { span: Span },
}

pub struct ElifChain {
    pub cond: Box<Cond>,
    pub guard: Option<Idx<Expr>>,
    pub body: Idx<Expr>,
    pub span: Span,
}

pub enum ArrayItem {
    Single(Idx<Expr>),
    Spread(Idx<Expr>),
}

pub enum ImportClause {
    Glob,
    Items(Vec<ImportItem>),
}
```

---

## Pratt Precedence Table (grammar-derived)

| BP | Operator(s) | Associativity | Token(s) |
|----|-------------|---------------|----------|
| 10 | assign `<-` | Right | `LtMinus` |
| 20 | `or` `xor` | Left | `Or` `Xor` |
| 30 | `and` | Left | `And` |
| 40 | `=` `/=` | None (1 use max) | `Eq` `SlashEq` |
| 50 | `<` `>` `<=` `>=` `in` | None | `Lt` `Gt` `LtEq` `GtEq` `In` |
| 60 | `..` `..<` | None | `DotDot` `DotDotLt` |
| 70 | `::` | Left | `ColonColon` |
| 80 | `\|` `^` | Left | `Pipe` `Caret` |
| 90 | `&` | Left | `Amp` |
| 100 | `shl` `shr` | Left | `Shl` `Shr` |
| 110 | `+` `-` | Left | `Plus` `Minus` |
| 120 | `*` `/` `%` | Left | `Star` `Slash` `Percent` |
| 130 | prefix `-` `not` `!` `@` `~` | — | |
| 140 | postfix call `.[` `.` `.{` `as` | — | |

**Non-associative operators** (`=`/`/=`, comparisons, ranges): parse right side at
`own_bp + 1` so a second occurrence fails to nest (produces `Expr::Error` on parse error).

**`<-` assign**: treated as infix at BP 10 producing `Expr::Assign { target, value }`.
Right-associative: parse right side at BP 9.

---

## Parser (`parser.rs`)

### Parser struct

```rust
pub struct Parser<'a> {
    tokens:  &'a [Token],
    pos:     usize,
    file_id: FileId,
    diags:   &'a mut DiagnosticBag,
    ctx:     ParseCtx,
}
```

Tokens must include a terminal `Eof`. Input is `&[Token]` (pre-collected from `Lexer`).

### Key LL(1) dispatch points (as per CLAUDE.md)

1. **`parse_expr_paren`** — after `(`:
   - next is `)` → Unit
   - else parse expr, then peek: `)` → Paren, `,` → Tuple, `;` → Block

2. **`parse_pat_ident`** — after ident:
   - next is `(` → positional sum PatSuffix
   - next is `{` → named-field PatSuffix
   - else → variable binding

3. **`parse_fn_kind`** — after `fn`:
   - next is `Ident` → named FnDef (letter/`_` disambiguates from `(`)
   - next is `(` or `[` → Lambda

4. **`parse_choice_body`** — after `{`:
   - optional leading `|`
   - then variant list separated by `|`

5. **`parse_expr_with_prefix`** — after optional attrs + modifiers, dispatch on keyword:
   `fn` / `record` / `choice` / `const` / `var` / `if` / `match` / `while` / `loop` /
   `for` / `label` / `return` / `break` / `cycle` / `defer` / `import` → dedicated rules
   Else → Pratt expression

6. **`parse_cond`** — if next is `case` → `Cond::Case { kind, pat, init }`; else → `Cond::Expr`

### Error recovery

- On unexpected token: emit diagnostic, advance past the offending token or to the next
  sync point (`;`, `)`, `}`, `]`, `Eof`), return `Expr::Error { span }`.
- Never bail — always produce a (possibly partial) AST.
- `Error` nodes suppress cascading diagnostics at semantic level.

### Public API

```rust
pub fn parse(
    tokens: &[Token],
    file_id: FileId,
    diags: &mut DiagnosticBag,
) -> ParsedModule
```

---

## S-Expression Dumper (`sexpr.rs`)

Deterministic, diff-friendly text representation for testing.

```
(fn_def main []
  (block
    (call (ident writeln) [(lit_str "Hello, world!")])))
```

```rust
pub fn dump(module: &ParsedModule, interner: &Interner) -> String
```

Walks the AST recursively. Every node type has a distinct prefix keyword.
Symbols are resolved via `interner.resolve(sym)`.

---

## Milestone

1. Parse a program using every grammar construct (record, choice, fn, match, if/elif/else,
   for, while, loop, label, import, all operators, all literal types).
2. Parse broken input → diagnostics + partial AST with `Error` nodes.
3. `dump` → re-parse the original → `dump` again → identical output (idempotent via the
   source, not re-parsing the s-expr).
4. `cargo test -p musi_parse` passes with 0 clippy warnings.
