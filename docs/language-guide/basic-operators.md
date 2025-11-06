# Basic Operators

Symbols carry meaning in Musi. Learning them helps you read code quickly. This chapter shows the main operators, explains where Musi matches Swift or TypeScript, and notes where it is different.

## Arithmetic at a Glance

```musi
const squared := value^2;
const remainder := total mod chunk;
```

`^` performs exponentiation and must hug its operands. Writing `value ^ 2` with spaces is a parse error. Swift uses the `pow` function ([Swift Standard Library](https://developer.apple.com/documentation/foundation/pow(_:_:))), and TypeScript uses `**` or `Math.pow` ([MDN Math.pow](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/Math/pow)), so Musi's special operator keeps maths short.

`mod` computes Euclidean division. Results are always non-negative, matching Rust's [`rem_euclid`](https://doc.rust-lang.org/std/primitive.i32.html#method.rem_euclid) behaviour. [Swift's `%`](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/basicoperators/#Remainder-Operator) and [TypeScript's `%`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Operators/Remainder) mirror truncated division, so negative dividends produce negative results. When you move code from another language, check the remainder logic so results stay correct.

Other arithmetic operators look familiar: `+`, `-`, `*`, `/`.

## Comparison and Equality

Musi treats `=` as equality instead of assignment, mirroring Swift's `==` but saving a character. Inequality reads as `=/=`, and relational operators stay the same (`<`, `<=`, `>`, `>=`).

## Logical and Bitwise Words

Logical operators use words instead of symbols:

```musi
const ready := is_valid and then is_cached;
const fallback := primary or else backup;
```

`and` and `or` evaluate both sides, while `and then` and `or else` short-circuit. TypeScript and Swift rely on `&&` and `||`; Musi spells words out to keep intent clear. Bitwise operations reuse the same words but work on integer values. Operand types decide whether `and` is logical or bitwise, and the type checker blocks invalid mixes.

## Pointer and Range Helpers

```musi
const ptr := &buffer;
const first := ptr.*;

for index in 0..<count {
  work(index);
}
```

`&` takes an address, `.*` dereferences it. Range operators match Swift: `..` includes the end, `..<` stops before it. Helper libraries in TypeScript use similar range ideas, so the concept should feel familiar.

## Swift and TypeScript Parallels

| Feature | Musi | Swift | TypeScript |
|---------|------|-------|------------|
| Power | `value^3` | `pow(value, 3)` | `value ** 3` |
| Euclidean remainder | `value mod 5` | `value % 5` (sign follows dividend) | `value % 5` (sign follows dividend) |
| Short-circuit | `a and then b` | `a && b` | `a && b` |
| Range | `0..<n` | `0..<n` | `Array.from({ length: n }, (_, i) => i)` |

## Recap

- `^` handles power directly and stays attached to its operands.
- `mod` returns non-negative remainders, so ported code may need tweaks.
- Word-based logical and bitwise operators keep intent obvious.
- Range, address-of, and dereference operators mirror tools you may already know.

Continue to [Pattern Matching](pattern-matching.md) to see how control flow builds on these operators.

### Further Reading

- [Swift Foundation – `pow`](https://developer.apple.com/documentation/foundation/pow(_:_:))
- [MDN Web Docs – `Math.pow()`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/Math/pow)
- [Rust Standard Library – `rem_euclid`](https://doc.rust-lang.org/std/primitive.i32.html#method.rem_euclid)
- [Swift Programming Language – Remainder (`%`) Operator](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/basicoperators/#Remainder-Operator)
- [MDN Web Docs – Remainder (`%`) operator](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Operators/Remainder)
