# Musi Style Guide

> Canonical formatting rules for Musi source code.
> Inspired by Google Java Style Guide, F# style conventions, and modern formatters.

---

## Overview

This guide defines the official formatting style for Musi source code. The LSP formatter and any future `musi fmt` tool should enforce these rules.

---

## Indentation & Whitespace

| Rule | Value | Rationale |
|------|-------|-----------|
| **Indent style** | Tabs | Accessibility (configurable width), smaller file size |
| **Indent width** | 2 spaces equivalent | Readable nesting depth |
| **Line width** | 100 characters | Modern wide screens; balance readability |
| **Trailing newline** | Required | POSIX compliance, clean diffs |
| **Trailing whitespace** | Forbidden | Clean diffs |

### Tab vs Space Context

- **Indentation**: Tabs only
- **Alignment**: Spaces only (within a line, after tab indentation)

---

## Braces & Blocks

### Brace Style: K&R (One True Brace)

Opening brace on same line as declaration, closing brace on own line.

```musi
fn calculate(x: Int): Int {
  if x > 0 {
    x * 2
  } else {
    0
  }
};

record Point {
  x: Int;
  y: Int;
};

choice Option[T] {
  case Some(value: T),
  case None,
};
```

### Single-Expression Bodies

Short expressions may remain on same line:

```musi
fn double(x: Int): Int { x * 2 };
```

---

## Semicolons

> [!IMPORTANT]
> **ALL statements require a semicolon.** There are no optional semicolons in Musi.
> A top-level expression wrapped in a statement is a `StmtExpr` and must end with `;`.

| Context | Rule |
|---------|------|
| **All statements** | **Required** (mandatory) |
| **Record/choice fields** | Required |
| **Top-level declarations** | Required after `}` |

```musi
fn example() {
  val x := 1;      // statement: semicolon required
  val y := 2;      // statement: semicolon required
  x + y            // final expression (not a statement): no semicolon
};                 // top-level fn is a statement: semicolon required
```

---

## Types vs Return Types

> [!IMPORTANT]
> **Function types** use `->` (like OCaml/F#): `Int -> String`
> **Function return types** use `: T`: `fn foo(): Int { ... }`

```musi
// Function return type uses :
fn greet(name: String): String {
  $"Hello, {name}"
};

// Function TYPE uses ->
val callback: Int -> String := fn(x) { x.to_string() };

// Higher-order function
fn apply(f: Int -> Int, x: Int): Int {
  f(x)
};
```

---

## Quotes

| Type | Style |
|------|-------|
| **Strings** | Double quotes `"hello"` |
| **Runes (chars)** | Single quotes `'x'` |
| **Template literals** | `$"Hello, {name}"` |

---

## Operators & Spacing

### Binary Operators

Spaces around all binary operators:

```musi
val result := a + b * c;
val check := x > 0 and y < 10;
val range := 1..10;
```

### Pipe Operator

The `|>` operator chains operations (like F#/Elixir):

```musi
val result := items
  |> filter(fn(x) => x > 0)
  |> map(fn(x) => x * 2)
  |> collect();
```

### Unary Operators

No space between operator and operand:

```musi
val neg := -x;
val deref := ptr.^;
val inverse := not flag;
```

### Type Annotations

Space after colon:

```musi
val x: Int := 0;
fn greet(name: String): String { ... };
```

### Assignment

Spaces around `:=` and `<-`:

```musi
val x := 1;
x <- x + 1;
```

---

## Commas & Separators

### Trailing Commas

**Required** in multi-line constructs:

```musi
val point := Point.{
  x := 1,
  y := 2,     // trailing comma
};

choice Color {
  case Red,
  case Green,
  case Blue,  // trailing comma
};
```

**Optional** in single-line:

```musi
val tuple := (1, 2, 3);  // no trailing comma needed
```

### Space After Comma

Always one space after comma:

```musi
fn add(a: Int, b: Int): Int { a + b };
```

---

## Naming Conventions

| Item | Style | Example |
|------|-------|---------|
| **Functions** | `snake_case` | `calculate_total` |
| **Variables** | `snake_case` | `user_count` |
| **Constants** | `SCREAMING_SNAKE` | `MAX_SIZE` |
| **Types** | `PascalCase` | `UserProfile` |
| **Type parameters** | `PascalCase`, often single letter | `T`, `Key`, `Value` |
| **Sum cases** | `PascalCase` | `Some`, `None` |
| **Modules** | `snake_case` | `file_system` |

---

## Blank Lines

| Context | Blank Lines |
|---------|-------------|
| Between top-level declarations | 1 |
| Between logical sections in a function | 1 |
| Inside record/choice bodies | 0 |
| Before `else`/`else if` | 0 |

```musi
fn first() { ... };

fn second() { ... };

record Point {
  x: Int;
  y: Int;
};
```

---

## Imports

### Syntax

```musi
import "std/io";
import "path/to/module";
```

### Grouping

Group imports logically, separated by blank lines:

1. Standard library
2. External dependencies
3. Internal modules

```musi
import "std/io";
import "std/collections/list";

import "external/json";

import "./utils";
import "./models";
```

---

## Comments

### Line Comments

Use `//` with one space after:

```musi
// This is a comment
val x := 1;  // inline comment
```

### Doc Comments

Use `///` for documentation:

```musi
/// Calculate the factorial of n.
/// Returns 1 for n <= 1.
fn factorial(n: Int): Int => if n <= 1 { 1 } else { n * factorial(n - 1) };
};
```

---

## Match Expressions

### Arm Formatting

```musi
match value {
case Some(x) => x * 2,
case None => 0,
};
```

### Guards

```musi
match value {
case x if x > 0 => "positive",
case x if x < 0 => "negative",
case _ => "zero",
};
```

---

## Long Lines

### Breaking Strategy

1. Break after `=>` in match arms
2. Break after `{` in function calls with closures
3. Break before `|>` in pipe chains
4. Break after binary operators (operator on previous line)

```musi
val result := some_long_function_name(
  first_argument,
  second_argument,
  third_argument,
);

val processed := items
  |> filter(fn(x) => x > 0)
  |> map(fn(x) => x * 2)
  |> collect();
```

---

## Configuration

Format configuration in project's `mspackage.json` (see [schema](file:///Users/krystian/CodeProjects/musi/schemas/mspackage-schema.v1.json)):

```json
{
  "fmt": {
    "useTabs": true,
    "indentWidth": 2,
    "lineWidth": 100,
    "proseWrap": "preserve"
  }
}
```

---

## Rationale Summary

| Choice | Reason |
|--------|--------|
| Tabs for indent | Accessibility, user preference |
| K&R braces | Vertical space efficiency |
| Trailing commas | Clean diffs, easy reordering |
| Double quotes | Consistency (single for runes) |
| 100 char lines | Modern displays, readable |
| Mandatory semicolons | Unambiguous statement boundaries |
