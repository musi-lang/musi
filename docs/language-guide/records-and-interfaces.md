# Records

Records store structured data with value semantics. Fields stay visible, copying is explicit, and there is no hidden inheritance tree to worry about. Procedures provide behaviour while records focus on shape.

## Declaring Records

```musi
const Point := record {
  x: Bin64 := 0.0;
  y: Bin64 := 0.0;
};
```

Fields list their types and can supply default values. Defaults run whenever you omit a field during construction.

## Building Values

```musi
const origin := Point{};                 // uses defaults
const point := Point{ .x := 3.0, .y := 4.0 };
```

Construction uses named fields so order never surprises you. Leave a field out and its default steps in.

## Updating Fields

Records are immutable unless you bind them with `var`:

```musi
var cursor := Point{};
cursor.x <- cursor.x + 1.0;
cursor.y <- cursor.y + 2.0;
```

Updates go through `<-`, keeping state changes obvious. To create a modified copy without mutation, rebuild the record:

```musi
const shifted := Point{ .x := point.x + 1.0, .y := point.y };
```

## Destructuring

Pattern matching pulls fields out quickly:

```musi
match point with {
case const Point{ .x, .y } -> draw(x, y),
};
```

Patterns always bind immutably. Rebind to `var` when you need mutation afterwards.

## Sharing Behaviour

Musi drops interfaces in favour of composition. You express shared logic with procedures that take records, optionally grouped in modules:

```musi
const magnitude := proc (p: Point) -> Bin64 {
 sqrt(p.x^2 + p.y^2)
};

const translate := proc (p: Point, dx: Bin64, dy: Bin64) -> Point {
 Point{ .x := p.x + dx, .y := p.y + dy }
};
```

Combine records by embedding them as fields, as shown in the [Composition](composition.md) chapter. This keeps data relationships explicit without interface hierarchies.

## Recap

- Records deliver value semantics with named fields and optional defaults.
- Construction always spells out field choices, avoiding positional confusion.
- Mutating a record requires `var` bindings; otherwise rebuild a new value.
- Pattern matching makes it easy to unpack fields in control flow.
- Shared behaviour lives in procedures and composition, not interfaces.
