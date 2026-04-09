# Syntax Reference

This document is the front-door overview of the current Musi surface syntax.

`grammar/Musi.g4` is the canonical, tool-supported grammar. `grammar/Musi.abnf` remains the compact spec reference.
`grammar/Musi.abnf` is strict RFC 5234 ABNF (kept aligned with `grammar/Musi.g4`).

## Core Shape

Musi is expression-driven.

- functions yield the value of their final expression
- top-level evaluation is the startup model
- effects are first-class
- typed positions reuse the ordinary expression grammar
- control flow is sequences + `case` + handlers (the grammar has no loop statements)

## Keywords

The language keeps these keyword families:

- binding and structure: `let`, `mut`, `rec`, `case`, `of`, `if`
- operator declarations: `infix`, `infixl`, `infixr`
- type and abstraction: `data`, `class`, `instance`, `law`, `where`, `forall`
- effects: `effect`, `perform`, `handle`, `with`, `resume`
- modules and interop: `import`, `export`, `foreign`, `opaque`, `as`
- metaprogramming: `quote`
- word operators: `and`, `or`, `xor`, `not`, `shl`, `shr`, `in`

`if` is a guard keyword. It is not a standalone `if/else` branching form.

## Bindings And Functions

Bindings use `:=`.

```musi
let x := 41;
let inc := \(n : Int) : Int => n + 1;
```

Mutable update uses `<-`.

```musi
let counter := mut 0;
counter <- 1;
```

`<-` writes value into target location. Borrow checking is not implemented.

`mut` has two separate roles:

- `mut expr` produces a writable location/value (`mut T` in type position), needed for writes through members and indices
- `let` bindings are immutable; mutation is performed by writing into locations

```musi
let x := mut 0;
x <- 1;

let array := mut [1, 2, 3];
array.[0] <- 4;
```

## Records

Record literals use `{ ... }`. Record update uses `expr.{ ... }`. Record patterns also use `{ ... }`.

```musi
let p := { x := 1, y := 2 };
let q := p.{ x := 3 };
let {x, y} := p;
```

## Imports

`import expr` is an expression. Aliasing and selective binding use `let` patterns. Path validation is semantic, not parser-only.

```musi
let IO := import "std/io";
let {read, write} := IO;
let dynamic := import module_path;
```

## Types

`Type`, `Type0`, `Type1`, ... are built-in names, not reserved syntax.

Bracket application is an ordinary postfix form, so type application is just one use of `expr[...]`:

```musi
Option[Int]
Result[Int, CString]
```

Indexing remains `expr.[...]`.

Anonymous sums use `+`.

```musi
Int + String
```

Functions use `->` and `~>` by kind, while declaration signatures carry effect rows through `with { ... }`.

## Data, Effects, And Classes

Named type and effect definitions are expressed through `let`.

```musi
let Option[T] := data { Some : T | None };

let Console := effect {
  let write (text : String) : Unit;
};

let Eq[T] := class {
  let (=) (a : T, b : T) : Bool;
  law reflexive (x : T) := x = x;
};

let eqInt := instance Eq[Int] {
  let (=) (a : Int, b : Int) : Bool := int_eq(a, b);
};
```

## Branching

Branching is centered on `case`.

```musi
case value of (
| .Some(x) => x
| .None => 0
);
```

Guards use `if` on case arms:

```musi
case x of (
| n if n > 0 => n
| _ => 0
);
```

## Operators

Structural operators include:

- `:=`
- `<-`
- `.`
- `?.`
- `!.`
- `:?`
- `:?>`
- `|>`
- `...`

Built-in word operators are:

- `and`
- `or`
- `xor`
- `not`
- `shl`
- `shr`
- `in` (set membership)

Users may define symbolic operators. Users may not define word operators.

Symbolic infix precedence/associativity is configured with fixity declarations:

```musi
infixl 6 (++);
infixr 5 (**);
infix 4 (<=>);
```

Parsing treats infix operators as a flat chain; fixity determines how the chain is folded into a tree.

## Recursion

Recursion uses `let rec`.

```musi
let rec fact (n : Int) : Int := /* ... */;
```

## Literals

Numeric literals:

- decimal: `123`, `1_000`
- hex: `0xFF`, `0xff_ff`
- octal: `0o755`, `0o7_5_5`
- binary: `0b1010`, `0b1_0_1_0`
- floats: `3.14`, `.5`, `2e10`, `.5e-2`

String literals use `"` and must not contain raw newlines. Multiline text uses template literals.

Template literals use backticks, may contain raw newlines, and may contain `${ expr }` interpolations:

```musi
let name := "world";
let msg := `hello ${name}`;
```

Rune literals use `'` and contain exactly one character (including via an escape).

Supported escapes in strings and runes:

- `\\`, `\"`, `\'`, ``\````, `\$`, `\n`, `\r`, `\t`, `\0`
- `\xHH` (2 hex digits)
- `\uXXXX` (4 hex digits) and `\uXXXXXX` (6 hex digits)

## FFI And Attrs

Foreign declarations use `foreign` with an optional ABI string:

```musi
@link(name := "c", symbol := "puts")
foreign "c" let puts (msg : CString) : Int;
```

Attrs are a separate surface from ordinary expressions. Public attrs, compiler-only attrs, and inert metadata are documented in the attrs reference.

## Metaprogramming

`quote` captures syntax as compile-time data.

```musi
quote (x + 1)
```

Splices use `#name`, `#(expr)`, and `#[exprs]` inside quoted syntax.
