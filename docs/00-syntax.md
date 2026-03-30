# Syntax Reference

This document is the front-door overview of the current Musi surface syntax.

`grammar.abnf` remains the canonical grammar. This file is the compact human guide to the same language.

## Core Shape

Musi is expression-driven.

- functions yield the value of their final expression
- top-level evaluation is the startup model
- effects are first-class
- loops, comprehensions, piecewise conditionals, and `return` are not part of the core language

## Keywords

The reduced core keeps these keyword families:

- binding and structure: `let`, `mut`, `case`, `of`, `if`
- type and abstraction: `data`, `class`, `instance`, `law`, `where`
- effects: `effect`, `perform`, `handle`, `with`, `resume`
- modules and interop: `import`, `export`, `foreign`, `opaque`, `as`
- metaprogramming: `quote`
- word operators: `and`, `or`, `xor`, `not`, `shl`, `shr`, `in`

`if` is a guard keyword. It is not a standalone `if/else` branching form.

## Bindings And Functions

Bindings use `:=`.

```musi
let x := 41;
let inc := (n : Int) : Int => n + 1;
```

Mutable update uses `<-`.

```musi
let mut counter := 0;
counter <- counter + 1;
```

`mut` has two separate roles:

- `let mut name := expr` marks binding as updatable via `<-`
- `mut expr` produces writable value (`mut T` in type position), needed for writes through members and indices

```musi
let mut x := 0;
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

`import "path"` is an expression. Aliasing and selective binding use `let` patterns.

```musi
let IO := import "std/io";
let {read, write} := IO;
```

## Types

Type application uses brackets:

```musi
Option[Int]
Result[Int, CString]
```

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
- `in`

Users may define symbolic operators. Users may not define word operators.

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
