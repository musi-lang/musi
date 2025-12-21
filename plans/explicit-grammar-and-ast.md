# Explicit Grammar and AST Proposal

This document outlines the proposed changes to the Musi grammar and AST to ensure explicitness, remove ambiguity, and support high-fidelity tooling (LSP, formatters).

## 1. Explicit Statement Termination

**Current State**: Somewhat inconsistent enforcement of semicolons in the parser.
**Proposal**: ALL expression-based statements MUST end with a semicolon.

```musi
// Valid
val x := 1;
writeln(x);

// Invalid
val x := 1
writeln(x)
```

> USER: Proposal accepted.

## 2. Unambiguous Record Literals

**Current State**: In the grammar, record literals use `.{`.
**Proposal**: Strictly enforce `.{` for record literals to distinguish them from blocks `{ ... }`.

```musi
// Record Literal
val p := Point.{ x := 1, y := 2 };

// Anonymous Record Literal
val a := .{ x := 10 };

// Block (not an expression unless it's the last element in a scope or used in a binding)
{
  val x := 1;
  x + 1;
};
```

> USER: Proposal accepted.

## 3. Pattern Matching: Or-Patterns

**Current State**: Grammar currently lacks an explicit `PatOr`.
**Proposal**: Support `PatOr` using the `|` separator.

```musi
match value {
case A | B | C => handle_abc();
case D => handle_d();
}; // USER: you forgot semicolon here! (top-level StmtExpr+ExprMatch means enforced semi!)
```

> USER: Proposal accepted.

## 4. Strict List Separators

**Current State**: Lenient parsing for many lists.
**Proposal**: Generic list parsing MUST require separators (usually `,`). No implicit "space-separated" lists.

```musi
// Invalid
fn add(a: Int32 b: Int32) ...
val x := [1 2 3];

// Valid
fn add(a: Int32, b: Int32) ...
val x := [1, 2, 3];
```

> USER: Proposal accepted.

## 5. Rich AST for Tooling (ESTree-inspired)

**Current State**: AST nodes only wrap the "kind" in a `with_span` object.
**Proposal**: Use prefix-shorthand (`Expr`, `Ty`, `Pat`, `Stmt`) and explicit wrappers for delimiters and separators to preserve full source fidelity.

### Generic Wrappers

```ocaml
type 'a spanned = {
  kind : 'a;
  span : Span.t;
}

(* Metadata for delimiters like (), [], {}, .{} *)
type 'd delimited = {
  ldelim : Span.t;
  value : 'a;
  rdelim : Span.t;
}

(* Metadata for separated lists (elements + their separators) *)
type ('a, 's) separated = {
  elems : 'a list;
  sep_spans : Span.t list; (* Spans of the commas/semicolons *)
}
```

> USER: Proposal accepted.

### Example: Function Call

```ocaml
| ExprCall of {
    callee: expr;
    args : (expr, Token.t) separated delimited;
  }
```

This allows the LSP to know exactly where the parentheses and commas are, enabling features like "rename argument" or "highlight separator" without re-lexing.

## 6. Symmetery between Binding and Assignment

**Proposal**:

- `:=` for **Binding** (introducing a new name).
- `<-` for **Assignment** (mutating an existing name).
- `=` for **Equality** (comparison).

```musi
val x := 1;        // Binding
x <- 2;            // Assignment
if x = 2 { ... };  // Equality
```

> USER: Proposal accepted (already existed as design)

---

## Discussion Points

- **Trailing Comma**: Should we allow trailing commas in all delimited lists? (Suggestion: Yes, for easier multi-line diffs).
  - > USER: Suggestion accepted via boolean flag (future: fmt setting)
- **Match Case Comma**: Should commas between `match` cases be required? (Proposal: Yes, to maintain list consistency).
  - > USER: Proposal accepted.
