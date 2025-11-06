# Error Handling

Musi handles errors through explicit `Expect<T, E>` types instead of exceptions. Every fallible operation returns a value that tracks success or failure, keeping error paths visible in function signatures.

## Expect Types

```musi
const Expect := choice Expect<T, E> {
  case Pass(T),
  case Fail(E),
};
```

`Expect<T, E>` wraps either a success value of type `T` or an error of type `E`. Functions that can fail return `Expect` explicitly:

```musi
const parse_int := proc (text: Text) -> Expect<Int, ParseError> {
  // implementation
};
```

Optional sugar `T?` exists for `Option<T>`, but `Expect<T, E>` stays explicit to keep error types visible.

## Propagating Errors with try

```musi
const load_config := proc (path: Text) -> Expect<Config, Error> {
  const file := try open_file(path);
  const text := try file.read();
  try parse_config(text)
};
```

`try` unwraps `Expect` values. If the expression evaluates to `.Fail(e)`, the enclosing procedure returns `.Fail(e)` immediately. Otherwise, execution continues with the unwrapped value. Errors flow through return values, keeping control flow explicit.

## Force Unwrapping with `!`

```musi
const value := result!;
```

`!` forces unwrapping. If `result` is `.Fail(e)`, the program panics. Use this when failure represents a programming error or when you have external guarantees about success:

```musi
const builtin_config := parse_config(EMBEDDED_CONFIG)!;  // known valid
```

Avoid `!` for user input or external data. Prefer `try` or explicit pattern matching.

## Pattern Matching Errors

```musi
const result := fetch_data();
match result with {
case .Pass(const data) -> process(data),
case .Fail(const err) -> {
  log_error(err);
  fallback_data()
},
}
```

Pattern matching gives you full control over error handling. Extract both success and failure values, then decide how to proceed.

### Conditional Error Checks

```musi
if case .Fail(const err) := validate(input) {
  writeln(`validation failed: ${err}`);
  return .Fail(err);
}
```

Use `if case` when you only care about one branch. This keeps error checks concise without nested `match` expressions.

## Defining Error Types

```musi
const ParseError := choice ParseError {
  case InvalidSyntax(line: Nat, col: Nat),
  case UnexpectedEOF,
  case UnknownToken(token: Text),
};
```

Error types are ordinary choice types. Include context that helps callers understand and recover from failures.

### Error Context

```musi
const read_config := proc (path: Text) -> Expect<Config, FileError> {
  const file := match open_file(path) with {
    case .Pass(const f) -> f,
    case .Fail(const e) -> return .Fail(.CannotOpen(path, e)),
  };
  // continue processing
};
```

Wrap lower-level errors with additional context. This builds error chains that preserve information as errors propagate.

## Recap

- `Expect<T, E>` makes error paths explicit in function signatures.
- `try` propagates errors upward, returning early on failure.
- `!` forces unwrapping and panics on error.
- Pattern matching gives full control over error handling.
- Error types are ordinary choice types with context.

Continue to [Optional Chaining](optional-chaining.md) to see how `Option<T>` and `?` work together.
