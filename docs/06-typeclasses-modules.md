# §6 — Typeclasses & Modules

## 6.1 Typeclass Declaration

```musi
class Eq over 'T {
    let (=)(a: 'T, b: 'T) : Bool;
    let (/=)(a: 'T, b: 'T) : Bool;

    law reflexivity  := forall a -> a = a;
    law symmetry     := forall a b -> (a = b) = (b = a);
    law transitivity := forall a b c -> (a = b) and (b = c) = (a = c);
}
```

`class` declares. `over 'T` names the type parameter. `law` declares semantic contracts — not runtime-enforced by default, used for compiler optimisation and proof tools.

## 6.2 Typeclass Instance

```musi
given Eq of Int {
    let (=)  := (a, b) -> ...;
    let (/=) := (a, b) -> not (a = b);
}

given Eq of List over 'T where 'T <: Eq {
    let (=) := (xs, ys) -> ...;
}
```

**Orphan rule**: a `given` must be in the same file as either the `class` or the target type. Violation is a compile error.

## 6.3 Superclass Constraints

```musi
class Ord over 'T where 'T <: Eq {
    let (<)(a: 'T, b: 'T) : Bool;
    let (<=)(a: 'T, b: 'T) : Bool;
    let (>)(a: 'T, b: 'T) : Bool;
    let (>=)(a: 'T, b: 'T) : Bool;
}
```

## 6.4 Operator Overloading

Via typeclass only. `and`/`or`/`not`/`xor` are **not overloadable**.

```musi
class Add over 'T {
    let (+)(a: 'T, b: 'T) : 'T;
}

given Add of Vector2 {
    let (+) := (a, b) -> Vector2.{ x := a.x + b.x, y := a.y + b.y };
}
```

## 6.5 Key Stdlib Typeclasses

### Into (coercion)

```musi
class Into over 'A 'B { let into(a: 'A) : 'B }

let f : Float64 := 5.into();
let s : String  := 42.into();
```

No implicit coercion anywhere. Stdlib aliases: `.toInt` `.toFloat64` `.toString` etc.

### Propagate (try desugaring)

```musi
class Propagate over 'F 'T 'E {
    let extract(f: 'F) : 'T ~> under { Throw of 'E };
}
```

`try expr` desugars to `Propagate.extract(expr)`. Stdlib provides instances for `Result` and `Option`.

### Iterable (no loop keywords)

```musi
class Iterable over 'T {
    let next(iter: 'T) : ?'T ~> under { State };
}

xs.map((x) -> x * 2);
xs.fold(0, (acc, x) -> acc + x);
xs.each((x) -> print(x));
```

## 6.6 Modules

Every `.ms` file is automatically a module. Nothing is public without `export`.

### Naming

```text
token.ms         →  Token
http_client.ms   →  HttpClient      // snake_case → PascalCase
parser/expr.ms   →  Parser.Expr     // directory = namespace
```

Override: `#[module := "Name"]`.

### Export

```musi
export let PI := 3.14159;           // inline
export { PI, sqrt };                // collected
export { sqrt as squareRoot };      // renamed
export { map, filter } := import "collections"; // re-export
```

### Import

`import` returns a record value — it is an expression, not a directive.

```musi
let Math         := import "math";
let { sqrt, PI } := import "math";  // destructured
```

### First-class modules

```musi
let compute := (math: { sqrt: Float64 -> Float64 }) -> math.sqrt(4.0);
compute(import "math");
compute(import "fastmath");
```

### Import order

Strictly top-to-bottom. Circular imports are a compile error. Dependency graph is a strict DAG. (F# model.)

## 6.7 Entry Point

```musi
#[entrypoint]
let run := () ~> () under { IO } -> ( ... );
```

No reserved `main`. `#[entrypoint]` marks the module's entry point. Duplicate `#[entrypoint]` is a compile error.
