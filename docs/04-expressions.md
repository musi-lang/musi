# §4 — Expressions

## 4.1 Everything Is an Expression

`let`, `var`, `class`, `given`, `match`, `return`, `defer` — all return a value (usually `()`). There is no statement/expression split. A statement is an expression followed by `;`.

## 4.2 Parenthesised Forms

The delimiter determines the form — one token of lookahead suffices.

| Form | Meaning |
|------|---------|
| `()` | Unit value |
| `(,)` | Empty tuple |
| `(;)` | Empty sequence — evaluates to `()` |
| `(a)` | Parenthesised expression |
| `(a,)` | Single-element tuple |
| `(a, b)` | Two-element tuple |
| `(a; b)` | Sequence — returns `b` |
| `(a; b;)` | Sequence — trailing `;` discards last, returns `()` |

## 4.3 Operator Precedence

Boolean-algebra model (VHDL/Pascal). Comparisons sit **below** logical operators. Do not apply C precedence.

| BP   | Operator(s)              | Assoc | Notes |
|------|--------------------------|-------|-------|
| 0010 | atoms                    | —     | literals, identifiers, `()` forms |
| 0020 | `. ?. () .[] .{}`        | Left  | field, call, index, record update |
| 0030 | `- not` (prefix)         | Right | arithmetic negation; logical/bitwise negation |
| 0040 | `* / %`                  | Left  | multiplicative |
| 0050 | `+ -`                    | Left  | additive |
| 0060 | `<< >>`                  | Left  | shift |
| 0070 | `::`                     | Right | cons |
| 0080 | `.. ..<`                 | None  | range |
| 0090 | `and`                    | Left  | conjunction — tightest logical |
| 0095 | `xor`                    | Left  | exclusive or |
| 0100 | `or`                     | Left  | disjunction — loosest logical |
| 0110 | `= /= < > <= >= in`      | None  | comparison and membership |
| 0120 | `\|>`                    | Left  | pipe |
| 0130 | `??`                     | Right | nil coalesce |
| 0140 | `<-`                     | Right | mutation — `var` only |

> `a and b = c` parses as `(a and b) = c`. Logical operators bind tighter than comparisons.

## 4.4 Bindings

```
let x := 5;                            // immutable, fixed
var x := 5;                            // mutable, rebindable
let p := ref Point.{ x := 1, y := 2 }; // heap, fixed binding
var p := ref Point.{ x := 1, y := 2 }; // heap, rebindable
```

`let`/`var` in destructuring — mutability is per-name:

```
let (a, b)    := (1, 2);   // both immutable
let (var a, b) := (1, 2);  // a mutable, b immutable
```

ML-style scoped binding:

```
let result := let x := expensive() in (x * 2);
```

## 4.5 Mutation

`<-` is the only mutation operator. Only valid on `var` bindings and through `ref` objects.

```
var x := 5;
x <- 10;

let p := ref Point.{ x := 1 };
p.x <- 2;    // always valid through ref
```

## 4.6 Piecewise — Sole Conditional Form

No `if/else`. No ternary. All conditional logic uses piecewise.

```
let fib := (n: Int) -> (
    0                     if n = 0
  | 1                     if n = 1
  | fib(n-1) + fib(n-2)   if _
);
```

Rules: result **first**, guard after `if`, arms separated by `|`, `_` is unconditional fallback (must be last), must be exhaustive.

## 4.7 Match

```
match expr (
    pattern if guard => result
  | pattern          => result
  | _                => fallback
);
```

| Pattern | Example | Notes |
|---------|---------|-------|
| Wildcard | `_` | matches anything, binds nothing |
| Literal | `0` `"x"` | exact match |
| Bind | `x` | immutable bind |
| Mutable bind | `var x` | mutable bind |
| Variant | `.Some(v)` | sum variant |
| Record | `{ x, y }` | named fields |
| Tuple | `(a, b)` | positional |
| Array | `[h, ...t]` | head and tail |
| Or | `p or q` | either pattern |

## 4.8 Functions

```
let add   := (x: Int, y: Int) -> x + y;
let fetch := (url: Url) ~> String over { IO } -> ...;
let id    := forall 'T -> (x: 'T) -> 'T -> x;
```

`->` pure. `~>` effectful — requires `over { ... }`. Functions are anonymous; the name comes from the enclosing binding.

### Parameter modes

| Mode | Syntax | Semantics |
|------|--------|-----------|
| Immutable | `(x: T)` | local copy, immutable |
| Mutable local | `(var x: T)` | local copy, mutable — changes do not escape |
| Copy-in copy-out | `(inout x: T)` | changes written back to caller on return |
| Heap ref | `(x: ref T)` | shared heap object — changes always visible |

`inout` is an Ada-style **parameter mode** — precedes the name, not the type. The type of `x` inside the function is `T`, not `inout T`. Call sites are clean — no annotation required:

```
let swap := (inout a: 'T, inout b: 'T) -> ...;
swap(x, y);   // clean call — mutation visible in signature only
```

Default parameter values:

```
let connect := (host: String, port: Int := 8080) -> ...;
connect("localhost");        // port uses default
connect("localhost", 9090);  // port overridden
```

Partial application with `...` hole:

```
let add5 := add(5, ...);
let add5 := add(..., 5);
```

## 4.9 UFCS

Any function whose first parameter matches the receiver type may be called with dot syntax. Resolved by the type checker — no runtime dispatch.

```
sqrt(4.0);
4.0.sqrt();   // identical semantics
```

## 4.10 UDN

Dot-prefix constructs variants and record literals. In pattern position, dot-prefix destructs.

```
.Some(5)                        // construct
Point.{ x := 1, y := 2 }       // named record literal
.{ x := 1, y := 2 }            // anonymous record literal

match opt (
    .Some(v) => v * 2
  | .None    => 0
);
```

`.None` ≠ `None`. `.None` is a variant constructor. `None` is an identifier lookup.

## 4.11 try

```
let v := try divide(a, b);
```

Unwraps `Ok`/`Some`, propagates `Err`/`None` upward. Resolved via the `Propagate` typeclass — works on any type implementing it.

## 4.12 defer

```
defer f.close();                    // single expression
defer (
    f.close();
    g.close();
);                                  // sequence block — LIFO as a unit
```

Runs at scope exit regardless of exit path. Multiple defers run LIFO. Cannot appear in tail position — verifier rejects `inv.tal` with pending defers in scope.

## 4.13 String Interpolation

```
f"hello {name}, you are {age} years old"
```

Expression inside `{ }` must have an `Into of String` instance.
