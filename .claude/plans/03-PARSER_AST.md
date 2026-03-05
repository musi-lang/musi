# Phase 3 — Parser + AST

**Crate:** `musi_parse`
**Goal:** Complete recursive-descent LL(1) parser producing a full AST, plus AST type definitions.
**Dependencies:** Phase 1 (musi_shared), Phase 2 (musi_lex)

---

## Deliverables

### AST Types

All nodes carry `Span`. Recursive children are `Idx<T>` (arena-allocated).

**Module: `ast::ty`**

```
Ty =
  | Arrow { params: Vec<Ty>, ret: Idx<Ty> }
  | Named { name: Symbol, type_args: Vec<Ty> }
  | Prod { elements: Vec<Ty> }          // (A, B, C)
  | Arr { element: Idx<Ty>, size: Option<Idx<Expr>> }
  | Var { name: Symbol }                 // 'a
```

**Module: `ast::pat`**

```
Pat =
  | Ident { name: Symbol, suffix: Option<PatSuffix> }
  | Lit { value: LitValue }
  | Wild                                  // _
  | Prod { elements: Vec<Pat> }
  | Arr { elements: Vec<Pat>, rest: Option<Symbol> }
  | AnonRec { fields: Vec<PatField> }
  | Or { alternatives: Vec<Pat> }

PatSuffix =
  | Positional { args: Vec<Pat> }        // Name(p1, p2)
  | NamedField { fields: Vec<PatField> } // Name{ f: p }
```

**Module: `ast::expr`**

```
Expr =
  // Atoms
  | Lit { value: LitValue }
  | Ident { name: Symbol }
  | Unit                                  // ()
  | Paren { inner: Idx<Expr> }           // (e)
  | Tuple { elements: Vec<Expr> }        // (e1, e2, ...)
  | Block { stmts: Vec<Expr>, tail: Option<Idx<Expr>> }  // (s; s; e)
  | Array { items: Vec<ArrayItem> }
  | RecLit { base: Idx<Expr>, fields: Vec<RecLitField> }  // expr.{ fields }

  // Control flow
  | If { cond: Idx<Expr>, then_body: Idx<Expr>, elif_chains: Vec<ElifChain>, else_body: Option<Idx<Expr>> }
  | Match { scrutinee: Idx<Expr>, arms: Vec<MatchArm> }
  | While { cond: Idx<Expr>, body: Idx<Expr> }
  | Loop { body: Idx<Expr> }
  | For { pat: Pat, iter: Idx<Expr>, body: Idx<Expr> }
  | Label { name: Symbol, body: Idx<Expr> }
  | Return { value: Option<Idx<Expr>> }
  | Break { label: Option<Symbol>, value: Option<Idx<Expr>> }
  | Cycle { label: Option<Symbol> }
  | Defer { body: Idx<Expr> }
  | Import { items: Vec<ImportItem>, path: Symbol }

  // Declarations (expression-level in Musi)
  | Record { attrs: Vec<Attr>, name: Symbol, ty_params: Vec<TyParam>, fields: Vec<RecField> }
  | Choice { attrs: Vec<Attr>, name: Symbol, ty_params: Vec<TyParam>, variants: Vec<ChoiceVariant> }
  | FnDef { attrs: Vec<Attr>, name: Symbol, ty_params: Vec<TyParam>, params: Vec<Param>, ret_ty: Option<Ty>, body: Idx<Expr> }
  | Lambda { ty_params: Vec<TyParam>, params: Vec<Param>, ret_ty: Option<Ty>, body: Idx<Expr> }
  | Bind { kind: BindKind, pat: Pat, ty: Option<Ty>, init: Idx<Expr> }

  // Operators
  | Postfix { base: Idx<Expr>, op: PostfixOp }
  | Prefix { op: PrefixOp, operand: Idx<Expr> }
  | Binary { op: BinOp, lhs: Idx<Expr>, rhs: Idx<Expr> }
  | Assign { target: Idx<Expr>, value: Idx<Expr> }

  // Error recovery
  | Error
```

**Supporting types:**

```
LitValue = Int(i64) | Float(f64) | String(Symbol) | Char(char) | Bool(bool)
BindKind = Const | Var
BinOp = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Leq | Geq | And | Or | BitAnd | BitOr | BitXor | Shl | Shr | Range | RangeInc | Pipe | As | Is
PrefixOp = Neg | Not | BitNot | Deref | AddrOf
PostfixOp = Call(Vec<Expr>) | Index(Idx<Expr>) | Field(Symbol) | Try
ArrayItem = Single(Expr) | Spread(Expr)
RecLitField = Named { name: Symbol, value: Expr } | Spread(Expr)
MatchArm = { pat: Pat, guard: Option<Expr>, body: Expr }
ChoiceVariant = { attrs: Vec<Attr>, name: Symbol, payload: Option<VariantPayload> }
VariantPayload = Positional(Vec<Ty>) | Named(Vec<RecField>)
RecField = { name: Symbol, ty: Ty, default: Option<Expr> }
Param = { pat: Pat, ty: Option<Ty> }
TyParam = { name: Symbol, bounds: Vec<Ty> }
Attr = { name: Symbol, args: Vec<AttrArg> }
ImportItem = { name: Symbol, alias: Option<Symbol> }
ElifChain = { cond: Expr, body: Expr }
```

### Parser

One method per grammar rule. Recursive descent with Pratt-style expression parsing.

**Pratt precedence (13 levels, low to high):**

| Level | Operators | Assoc |
|-------|-----------|-------|
| 1 | `<-` (assign) | Right |
| 2 | `..` `..=` (range) | None |
| 3 | `\|>` (pipe) | Left |
| 4 | `or` | Left |
| 5 | `and` | Left |
| 6 | `==` `!=` | Left |
| 7 | `<` `>` `<=` `>=` `is` `as` | Left |
| 8 | `\|` (bitor) | Left |
| 9 | `^` (bitxor) | Left |
| 10 | `&` (bitand) | Left |
| 11 | `<<` `>>` (shift) | Left |
| 12 | `+` `-` | Left |
| 13 | `*` `/` `%` | Left |

Prefix: `not`, `-`, `~`, `!`, `@` (higher than any infix).
Postfix: `.field`, `.[index]`, `(args)`, `.{rec}` (highest precedence).

**Key LL(1) decision points:**

1. **`parse_expr_paren`**: After `(`, peek:
   - `)` → Unit
   - Otherwise: parse expr, then peek `)` (paren) / `,` (tuple) / `;` (block)

2. **`parse_pat_ident`**: After ident, peek:
   - `(` → positional sum pattern
   - `{` → named-field pattern
   - Otherwise → variable binding

3. **`parse_fn_kind`**: After `fn`, peek:
   - Ident (letter/`_`) → named function definition
   - `(` or `[` → lambda

4. **`parse_choice_body`**: After `{`, optional leading `|`, then variant list separated by `|`.

5. **`parse_expr_with_prefix`**: After optional attrs, peek keyword:
   - `fn` → fn_kind
   - `record` → record def
   - `choice` → choice def
   - `if` → if expr
   - `match` → match
   - `while` → while
   - `loop` → loop
   - `for` → for
   - `label` → label
   - `const`/`var` → bind
   - `return` → return
   - `break` → break
   - `cycle` → cycle
   - `defer` → defer
   - `import` → import
   - Otherwise → Pratt expression

### Error Recovery

- On unexpected token: emit diagnostic, skip tokens until sync point (`;`, `)`, `}`, `]`).
- Insert `Expr::Error` / `Pat::Error` / `Ty::Error` node.
- Continue parsing after sync point.
- Collect all diagnostics — don't bail on first error.

### S-Expression Dumper

Deterministic, diff-friendly text format for AST debugging.

```
(fn_def main []
  (block
    (call (ident writeln) [(lit_string "Hello, world!")])))
```

---

## Milestone

1. Parse a program using every grammar construct (record, choice, fn, match, if/elif/else, for, while, loop, label, import, all operators).
2. Parse broken input → diagnostics + partial AST (with Error nodes).
3. Parse → dump → parse → dump is idempotent.
4. `cargo test -p musi_parse` passes.
