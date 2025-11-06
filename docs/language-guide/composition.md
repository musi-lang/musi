# Composition

Musi uses composition instead of inheritance. Records embed other records directly, and procedures operate on data without class hierarchies. This keeps dependencies explicit and easy to reason about.

## Embedding Records

```musi
const Point := record {
  x: Bin64;
  y: Bin64;
};

const Circle := record {
  center: Point;
  radius: Bin64;
};
```

Embed records as fields. Access nested data through dot notation:

```musi
const c := Circle{ .center := Point{ .x := 0.0, .y := 0.0 }, .radius := 5.0 };
const cx := c.center.x;
```

No inheritance means no hidden method dispatch, no fragile base class problems, no diamond inheritance puzzles.

## Shared Behavior Through Procedures

```musi
const translate := proc (p: Point, dx: Bin64, dy: Bin64) -> Point {
  Point{ .x := p.x + dx, .y := p.y + dy }
};

const move_circle := proc (c: Circle, dx: Bin64, dy: Bin64) -> Circle {
  Circle{ .center := translate(c.center, dx, dy), .radius := c.radius }
};
```

Define procedures that work on specific types. Reuse logic by calling other procedures, not by inheriting methods.

## Polymorphism Through Choice Types

```musi
const Shape := choice Shape {
  case Circle(center: Point, radius: Bin64),
  case Rect(origin: Point, width: Bin64, height: Bin64),
};

const area := proc (s: Shape) -> Bin64 {
  match s with {
  case .Circle(const center, const r) -> 3.14159 * r^2,
  case .Rect(const origin, const w, const h) -> w * h,
  }
};
```

Choice types provide tagged unions. Pattern matching dispatches on variants explicitly, keeping all cases visible at call sites.

## Generic Procedures

```musi
const map := proc <T, U>(items: [T], f: proc (T) -> U) -> [U] {
  // implementation
};

const lengths := map(["hello", "world"], proc (s: Text) -> Nat { len(s) });
```

Generics let procedures work across types without inheritance. Type parameters stay explicit, and the compiler generates specialised versions.

## Why No Inheritance?

Inheritance couples types through implicit relationships. Subclasses depend on superclass implementation details, making changes fragile. Method dispatch hides control flow, complicating debugging and performance analysis.

Composition keeps dependencies explicit. You see exactly which data a type contains and which procedures operate on it. Refactoring moves fields and updates call sites, not method overrides.

## Recap

- Records embed other records as fields.
- Procedures provide shared behavior without inheritance.
- Choice types enable polymorphism through pattern matching.
- Generics work across types without class hierarchies.
- Composition keeps dependencies explicit and refactoring straightforward.

Continue to [Generics](generics.md) to see how type parameters work across procedures and types.
