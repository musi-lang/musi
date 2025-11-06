# Optional Chaining

Optional values appear everywhere: dictionary lookups, array bounds checks, nullable fields. Musi uses `Option<T>` to make absence explicit, then provides operators that keep optional handling concise.

## Option Types

```musi
const Option := choice Option<T> {
  case Some(T),
  case None,
};
```

`Option<T>` wraps values that might not exist. Functions return `Option` when absence is expected:

```musi
const find := proc <T>(items: [T], pred: proc (T) -> Bool) -> Option<T> {
  // implementation
};
```

### Optional Type Sugar

```musi
const maybe_user: User?;           // sugar for 'Option<User>'
const lookup: proc (Text) -> Int?; // sugar for 'proc (Text) -> Option<Int>'
```

`T?` reads as "optional T" and desugars to `Option<T>`. Use it in type annotations to keep signatures readable.

## Unwrapping with ?

```musi
const get_name := proc (id: Nat) -> Text? {
  const user := find_user(id)?;
  const profile := user.profile?;
  .Some(profile.name)
};
```

`?` unwraps `Option<T>` values. If the value is `.None`, the enclosing procedure returns `.None` immediately. Otherwise, execution continues with the unwrapped value.

Chain multiple `?` operators to handle nested optionals:

```musi
const city := user?.address?.city?;
```

Each `?` checks for `.None` and propagates it upward.

## Force Unwrapping with `!`

```musi
const value := optional_value!;
```

`!` forces unwrapping. If the value is `.None`, the program panics. Use this when you have external guarantees about presence:

```musi
const first := items[0]!;  // known non-empty
```

Avoid `!` for user input or external data. Prefer `?` or explicit pattern matching.

## Pattern Matching Optionals

```musi
match find_user(id) with {
  case .Some(const user) -> display(user),
  case .None -> writeln("User not found"),
}
```

Pattern matching gives you full control. Extract the wrapped value or handle absence explicitly.

### Conditional Checks

```musi
if case .Some(const user) := find_user(id) {
  writeln(`found: ${user.name}`);
}
```

Use `if case` when you only care about the `.Some` branch. This keeps optional checks concise.

## Default Values

```musi
const count := optional_count or else 0;
const name := user.name or else "Anonymous";
```

`or else` provides fallback values when optionals are `.None`. The right side evaluates only if needed, so expensive computations stay lazy.

## Chaining Operations

```musi
const result := find_user(id)
  .map(proc (u) -> Text { u.name })
  .map(proc (n) -> Text { n.uppercase() })
  .or_else(proc () -> Text { "Unknown" });
```

Standard library methods like `map` and `or_else` let you transform optionals without explicit unwrapping. These will be documented in the standard library reference.

## Recap

- `Option<T>` makes absence explicit; `T?` provides readable sugar.
- `?` unwraps optionals and returns `.None` on absence.
- `!` forces unwrapping and panics on `.None`.
- Pattern matching gives full control over optional handling.
- `or else` provides default values for absent optionals.

Continue to [Pattern Matching](pattern-matching.md) to see how patterns work across all control flow constructs.
