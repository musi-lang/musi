# Musi Compiler Diagnostics

## Structure

### Core Types

```ocaml
type level = Error | Warning | Note

type t = {
  level : level;
  message : string;
  span : Span.t;
  notes : (string * Span.t) list;
}

type diags = {
  items : t list;
  error_count : int;
  warning_count : int;
}
```

### Constructors

```ocaml
make : level -> string -> Span.t -> t
error : string -> Span.t -> t
warning : string -> Span.t -> t
note : string -> Span.t -> t
with_note : t -> string -> Span.t -> t
```

### Bag Operations

```ocaml
empty_bag : diags
is_empty : diags -> bool
has_errors : diags -> bool
add : diags -> t -> diags
to_list : diags -> t list
merge : diags list -> diags
error_count : diags -> int
warning_count : diags -> int
```

## Format

### Location

```text
file:line:column: severity: message
```

**Example:**

```text
test.ms:1:17: error: cannot convert type 'String' to expected type 'Int32'
```

### Source Display

```text
 1 | val x: Int32 := "hello";
   |                 ^^^^^^^
```

### Colors

- **Red**: errors
- **Yellow**: warnings
- **Cyan**: notes

## Message Style

**Rules:**

- Lowercase (except proper nouns: Musi, OCaml, etc.)
- No articles (the, a, an) unless necessary
- No contractions (cannot, not can't)
- Semicolon separates problem from hint

**Example:**

```text
error: missing ';' after value binding; add ';'
```

## Categories

### Lexical Errors

- Invalid token
- Unterminated string literal
- Invalid escape sequence

### Parse Errors

- Unexpected token
- Missing token (`;`, `)`, `}`)
- Invalid syntax

### Semantic Errors

- Type mismatch
- Undefined identifier
- Duplicate definition
- Invalid scope

## Result Type Integration

```ocaml
type 'a result = ('a * diags) Stdlib.result

try_ok : 'a -> 'a result
try_error_bag : diags -> 'a result
try_error_diag : t -> 'a result
try_error_info : string -> Span.t -> 'a result

try_bind : ('a -> 'b result) -> 'a result -> 'b result
try_map : ('a -> 'b) -> 'a result -> 'b result
try_map_error : (diags -> diags) -> 'a result -> 'a result
```

**Purpose**: Accumulate errors without early exit. Success carries both value and diagnostic bag.

## Format Strings

### Select Format

```text
%select{option1|option2|option3}%d
```

**Example:**

```text
%select{expected identifier|expected expression|expected type}%0
```

Yields: "expected identifier" (if index = 0)

### Pluralization

```text
%plural{1:item|:items}%d
```

**Example:**

- Count = 1 → "item"
- Count ≠ 1 → "items"

### Type Information

```text
(type 'String' and 'Int32')
```

## Error Recovery

### Parse Recovery

- **Skip token**: ignore and continue
- **Insert token**: assume required token present
- **Synchronize**: skip to statement boundary

### Type Recovery

- Assume default type (`Any`)
- Infer from context
- Use error type to suppress cascading errors

## Examples

### Type Mismatch

```musi
val x: Int32 := "hello";
```

**Output:**

```text
test.ms:1:17: error: cannot convert type 'String' to type 'Int32'
 1 | val x: Int32 := "hello";
   |                 ^^^^^^^
```

### Missing Semicolon

```musi
val x: Int32 := 42
val y: Int32 := x + 1;
```

**Output:**

```text
file.ms:1:19: error: expected ';' after value binding
 1 | val x: Int32 := 42
   |                   ^
```

### Undefined Variable

```musi
val result := undefined_var + 1;
```

**Output:**

```text
file.ms:1:15: error: undefined identifier 'undefined_var'; did you mean 'defined_var'?
 1 | val result := undefined_var + 1;
   |               ^^^^^^^^^^^^^
```

### Multiple Errors with Notes

```musi
val x: Int32 := "text";
val y: String := x + 1;
```

**Output:**

```text
file.ms:1:17: error: cannot convert type 'String' to type 'Int32'
 1 | val x: Int32 := "text";
   |                 ^^^^^^

file.ms:2:18: error: cannot convert type 'Int32' to type 'String'
 2 | val y: String := x + 1;
   |                  ^^^^^

file.ms:1:17: note: 'x' has type 'Int32'
 1 | val x: Int32 := "text";
   |     ^
```

## Testing Framework

### Embedded Expectations

```musi
// ERROR: expected ';' after value binding
val x: Int32 := 42
```

**Verification:**

1. Diagnostic appears at expected location
2. Message contains expected keywords
3. Hints match expected suggestions

### Test Runner Checks

- Location accuracy
- Message content
- Severity level
- Note presence and content

## Implementation Notes

### Storage

List of diagnostic entries with severity, message, span, hints, optional fix-its.

### Performance

- **Lazy evaluation**: computed only when needed
- **Caching**: repeated identical messages cached
- **Batching**: collect multiple diagnostics before display

## Common Patterns

### Type Inference Failure

```text
error: cannot infer type for 'x'; add type annotation
```

### Scope Error

```text
error: identifier 'x' not in scope
note: similar identifier 'y' found in parent scope
```

### Pattern Match Exhaustiveness

```text
warning: pattern match not exhaustive; missing case 'None'
```

### Duplicate Definition

```text
error: duplicate definition of 'x'
note: previous definition here
```

### Invalid Modifier Combination

```text
error: cannot combine 'export' and 'unsafe' modifiers
```

### FFI Type Mismatch

```text
error: FFI function 'malloc' expects type '^Unit' but got 'Int32'
note: unsafe blocks required for FFI calls
```
