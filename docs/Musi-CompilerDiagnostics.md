# Musi Compiler Diagnostics

## Structure

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
 1 | let x: Int32 := "hello";
   |                 ^^^^^^^
```

### Colors

- **Red**: errors
- **Yellow**: warnings
- **Cyan**: notes

## Message Style

**Rules:**

- Lowercase (except proper nouns: Musi, OCaml, etc.)
- No articles (the, a, an)
- No contractions (cannot, not can't)
- Semicolon separates problem from hint

**Example:**

```text
error: missing ';' after constant binding; add ';'
```

## Error Archetypes

Strict taxonomy for error messages inspired by Clang. Messages are composed from **prefix**, **infix**, and **suffix** parts. Vagueness (e.g., "invalid syntax") is forbidden.

### Prefix Archetypes

The **prefix** indicates what went wrong. Always use these exact words:

| Prefix        | When to Use                       | Example                                        |
| :------------ | :-------------------------------- | :--------------------------------------------- |
| `unknown`     | Entity exists but not recognized  | "unknown escape sequence '\\q'"                |
| `unclosed`    | Opening delimiter without closing | "unclosed block comment"                       |
| `expected`    | Parser needed something specific  | "expected identifier after 'val'"              |
| `unexpected`  | Parser encountered wrong token    | "unexpected token 'else'"                      |
| `missing`     | Required element absent           | "missing return type annotation"               |
| `invalid`     | Syntactically present but wrong   | "invalid rune literal"                         |
| `malformed`   | Structure is wrong/incomplete     | "malformed hex literal '0x'"                   |
| `illegal`     | Forbidden in this context         | "illegal character in identifier"              |
| `cannot`      | Semantic impossibility            | "cannot convert type 'String' to type 'Int32'" |
| `undefined`   | Name not in scope                 | "undefined identifier 'foo'"                   |
| `duplicate`   | Already defined                   | "duplicate definition of 'x'"                  |
| `unused`      | Declared but never used           | "unused variable 'temp'"                       |
| `unreachable` | Code will never execute           | "unreachable code after 'return'"              |

### Infix Archetypes

The **infix** connects subject to object. Used for type errors and conversions:

| Infix | Pattern                     | Example                                        |
| :---- | :-------------------------- | :--------------------------------------------- |
| `to`  | `cannot convert %0 to %1`   | "cannot convert type 'String' to type 'Int32'" |
| `as`  | `cannot use %0 as %1`       | "cannot use type 'Int32' as function"          |
| `<:`  | `cast failed: %0 is not %1` | "cast failed: Any is not Int32"                |
| `for` | `%0 for %1`                 | "missing argument for parameter 'x'"           |
| `in`  | `%0 in %1`                  | "duplicate field 'x' in record"                |
| `of`  | `%0 of %1`                  | "invalid member of 'Point'"                    |

### Suffix Archetypes

The **suffix** provides context or hints after semicolon:

| Suffix                 | When to Use        | Example                                   |
| :--------------------- | :----------------- | :---------------------------------------- |
| `; add %0`             | Suggest addition   | "missing type; add annotation"            |
| `; remove %0`          | Suggest removal    | "duplicate modifier; remove 'export'"     |
| `; did you mean '%0'?` | Suggest correction | "undefined 'prnt'; did you mean 'print'?" |

### Composing Messages

Combine prefix + subject + infix + object + suffix:

```text
<prefix> <subject> [<infix> <object>] [; <suffix>]
```

**Examples:**

```text
expected identifier after 'val'
cannot convert type 'String' to type 'Int32'
undefined identifier 'foo'; did you mean 'bar'?
unclosed block comment; missing '*/'
```

### Guidelines

1. **Specificity**: Never say "invalid syntax". Describe *what* is invalid.
2. **Quoting**: Always quote type names and identifiers with single quotes.
3. **Hints**: Use semicolon separator to offer fixes.
4. **Context**: Specify what kind of thing is unclosed/missing.

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
- Typeclass constraint not satisfied
- Instance method not found

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
let x: Int32 := "hello";
```

**Output:**

```text
test.ms:1:17: error: cannot convert type 'String' to type 'Int32'
 1 | let x: Int32 := "hello";
   |                 ^^^^^^^
```

### Missing Semicolon

```musi
let x: Int32 := 42
let y: Int32 := x + 1;
```

**Output:**

```text
file.ms:1:19: error: expected ';' after value binding
 1 | let x: Int32 := 42
   |                   ^
```

### Undefined Variable

```musi
val result := undefined_var + 1;
```

**Output:**

```text
file.ms:1:15: error: undefined identifier 'undefined_var'; did you mean 'defined_var'?
 1 | let result := undefined_var + 1;
   |               ^^^^^^^^^^^^^
```

### Multiple Errors with Notes

```musi
let x: Int32 := "text";
let y: String := x + 1;
```

**Output:**

```text
file.ms:1:17: error: cannot convert type 'String' to type 'Int32'
 1 | let x: Int32 := "text";
   |                 ^^^^^^

file.ms:2:18: error: cannot convert type 'Int32' to type 'String'
 2 | let y: String := x + 1;
   |                  ^^^^^

file.ms:1:17: note: 'x' has type 'Int32'
 1 | let x: Int32 := "text";
   |     ^
```

## Testing Framework

### Embedded Expectations

```musi
// ERROR: expected ';' after value binding
let x: Int32 := 42
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

### Scope Error

```text
error: cannot find name 'x' in scope
note: similar name 'y' found in parent scope
```

### Pattern Match Exhaustiveness

```text
warning: non-exhaustive pattern match; missing variant 'None'
```

### Duplicate Definition

```text
error: duplicate definition of 'x'
note: previous definition here
```

### Typeclass Constraint Not Satisfied

```text
error: type 'Point' does not satisfy interface 'Eq'
note: missing method 'eq: (self: Point, other: Point) -> Bool'
```

### Interface Method Signature Mismatch

```text
error: method 'eq' signature does not match interface 'Eq'
note: interface expects '(self: 'T, other: 'T) -> Bool', found '(self: 'T) -> Bool'
```

### Invalid Loop Label

```text
error: undefined loop label 'outer'
```

### Pattern Binding In Conditional

```text
error: pattern binding type mismatch
note: expected 'Option[T]' but found 'Result[T, E]'
```
