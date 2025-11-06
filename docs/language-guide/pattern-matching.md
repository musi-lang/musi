# Pattern Matching

Musi uses one pattern language everywhere. The same binding you place in a `match` arm works inside `if`, `while`, and `for`. This shared approach keeps control flow tidy while staying close to Swift's pattern syntax and TypeScript's discriminated unions (see [Patterns](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/patterns/) and [Narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)).

## What Patterns Solve

Patterns pull values apart safely. They unwrap options, pick fields out of tuples, and select choice cases without manual checks. Instead of writing nested conditionals, you describe the shape you expect and Musi verifies it for you.

```musi
const status := Expect.fetch();
match status {
case .Pass(const data) -> handle(data),
case .Fail(const err) -> report(err),
};
```

## Binding Basics

Every binding inside a pattern starts with `const`. This mirrors Swift's preference for immutable `let` and TypeScript's checked `const`. You can ignore values with `_` and still show intent.

```musi
if const value := reading {
  display(value);
}

match result {
case const _ -> writeln("Got something"),
case _ -> writeln("Empty"),
};
```

## Variants and Tuples

```musi
match point {
case const (x, y) -> draw_at(x, y),
case const (x, _) -> mark_x(x),
};

if case .Some(const item) := maybe_item {
  use(item);
}
```

Enum cases and tuple slots read naturally. Musi lets you choose between prefixing `const` before the pattern or sprinkling it inside, and both styles compile the same way. Stick with one per project to keep reviews calm.

## Adding Conditions

Attach guards with `if` clauses or chain multiple pattern checks with commas. Guards mirror Swift where clauses, while chained checks feel close to TypeScript's successive narrowing.

```musi
if case .Some(const size) := maybe_size, size > 0 {
  prepare(size);
}

match request {
case .Pass(const body) if body.is_json() -> handle_json(body),
case .Pass(const body) -> handle_text(body),
case .Fail(const err) -> report(err),
};
```

## Loops That Listen

`while` and `for` understand the same pattern language, which spares you from manual destructuring inside the loop body.

```musi
while case .Some(const item) := iterator.next() {
  process(item);
}

for case .Pass(const response) in responses {
  record(response);
}
```

## Mutability After Matching

Patterns always bind immutably. When you do need mutation, rebind to a `var` inside the scope. This keeps the pattern syntax short while making mutation explicit.

```musi
if case .Some(const data) := download() {
  var buffer := data;
  buffer <- normalise(buffer);
  store(buffer);
}
```

## Swift and TypeScript Parallels

| Feature | Musi | Swift | TypeScript |
|---------|------|-------|------------|
| Option unwrap | `if const value := maybe` | `if let value = maybe` | `if (maybe.kind === "some")` |
| Choice match | `match choice { case .Left -> ... }` | `switch choice { case .left: ... }` | `switch (choice.tag) { case "Left": ... }` |
| Guarded branch | `case const value if value > 0` | `case let value where value > 0` | `if (value !== undefined && value > 0)` |

## Recap

- One pattern language works across `if`, `while`, `for`, and `match`.
- Bindings default to `const`, so you rebind when mutation is required.
- Guards and chained conditions let you express complex flows without nested checks.

Next up, later chapters will cover Musi's text types, collections, and control flow in depth.

### Further Reading

- [Swift Programming Language – Patterns](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/patterns/)
- [TypeScript Handbook – Narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)
