# A Musi Tour

This tour shows Musi's flavour in a few short steps. Skip around if you like, but reading in order gives you a feel for how features connect.

## Bindings and Control Flow

```musi
const threshold := 10;
var total := 0;

while total < threshold {
  total <- total + 1;
}
```

Immutable bindings use `const`, mutable ones use `var`, and mutation always goes through `<-`. Conditions behave like expressions, so you can return values from blocks without extra syntax. Learn more in [The Basics](the-basics.md).

## Pattern Matching Everywhere

```musi
if case .Some(const value) := maybe_total, value > 0 {
  writeln(`Positive: ${value}`);
}
```

`if`, `while`, and `for` all understand patterns, just like `match`. Bind names with `const`, then rebind to `var` later if you need mutation. The [Pattern Matching](pattern-matching.md) chapter explores every pattern form.

## Operators That Read Like Code

```musi
const squared := value^2;
const remainder := total mod divisor;
```

`^` stays tight to its operands, and `mod` always returns a non-negative remainder. Logical keywords such as `and then` and `or else` keep intent clear. See [Basic Operators](basic-operators.md) for the full table.

## Records, Choices, and Interfaces

```musi
const Point := record {
  x: Bin64;
  y: Bin64;
};

const distance := proc (a: Point, b: Point) -> Bin64 {
  const dx := a.x - b.x;
  const dy := a.y - b.y;
  sqrt(dx * dx + dy * dy)
};
```

Records give you named fields, choices describe tagged unions, and interfaces specify shared behaviour. These topics will receive dedicated chapters as the book grows.

## Error Paths Without Exceptions

Musi favours explicit results:

```musi
const fetch_user := proc (id: Nat) -> User!Error {
  const response := http.get(`/users/${id}`)?;
  response.json()
};
```

Expect types (`T!E`) track success and failure without hidden control flow. A future chapter on error handling will cover patterns like `case .Fail` and early returns.

## Where to Go Next

Jump to [The Basics](the-basics.md) for concrete syntax rules, or browse the rest of the Language Guide from the [book index](../README.md).
