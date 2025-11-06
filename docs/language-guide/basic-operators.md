# Basic Operators

Symbols carry meaning in Musi. Learning them helps you read code quickly. This chapter shows the main operators, highlights changes from earlier drafts, and explains why each piece exists.

## Arithmetic at a Glance

```musi
const squared := value^2;
const remainder := total mod chunk;
```

`^` performs exponentiation and must hug its operands. Writing `value ^ 2` with spaces is a parse error, keeping the operator unambiguous.

`mod` computes Euclidean division. Results are always non-negative, even when the dividend is negative. When you port code from languages that truncate toward zero, double-check remainder logic so that it actually stays correct.

Other arithmetic operators look familiar: `+`, `-`, `*`, `/`.

## Comparison and Equality

Musi treats `=` as equality instead of assignment, saving a character while keeping the comparison obvious enough. Inequality reads as `=/=`, and relational operators stay the same (`<`, `<=`, `>`, `>=`).

## Logical and Bitwise Words

Logical operators use words instead of symbols:

```musi
const ready := is_valid and then is_cached;
const fallback := primary or else backup;
```

`and`, `or`, `not`, and `xor` evaluate both sides and operate on booleans or integers depending on context. `and then` and `or else` short-circuit just like Ada's phrasing: evaluation stops as soon as the result is known. Bitwise operations reuse the same words but act on integer values. Operand types decide whether an expression is logical or bitwise, and the type checker blocks invalid mixes.

## Pointer and Range Helpers

```musi
const ptr := addr(buffer);
const first := ptr.pointee;

unsafe {
  ptr.pointee <- 42;
  const moved := ptr.offset(3);
}

for index in 0..<count {
  work(index);
}
```

`addr(value)` takes an address, `.pointee` accesses the target, and `offset` moves a pointer by elements. Range operators stay expressive: `..` includes the end, `..<` stops before it.

## Recap

- `^` handles power directly and stays attached to its operands.
- `mod` returns non-negative remainders, so ported code may need tweaks.
- Word-based logical and bitwise operators keep intent obvious and make short-circuit phrases self-explanatory.
- `addr`, `.pointee`, and `offset` replace symbolic pointer operators while keeping intent precise.

Continue to [Pattern Matching](pattern-matching.md) to see how control flow builds on these operators.

## Unwrap and Propagation Operators

Musi provides explicit operators for working with `Option<T>` and `Expect<T, E>` types:

```musi
const value := maybe_result!;     // force unwrap, panics on '.None' or '.Fail'
const count := optional_count?;   // unwrap 'Option', returns early on '.None'
const data := try fetch_data();   // propagate 'Expect' error, returns early on '.Fail'
```

`!` forces unwrapping and panics if the value is `.None` or `.Fail`. Use it when you know a value must exist or when failure represents a programming error.

`?` unwraps `Option<T>` values, returning `.None` from the enclosing procedure if the value is absent. It works as a postfix operator so you can chain unwrapping steps.

`try` propagates `Expect<T, E>` errors upward. If the expression evaluates to `.Fail(e)`, the enclosing procedure returns `.Fail(e)` immediately. This replaces exception-based error handling with explicit control flow.

### Type Casting

```musi
const result := value as TargetType;      // safe cast, returns 'Option<TargetType>'
const forced := (value as TargetType)!;   // force cast, panics on failure
const checked := (value as TargetType)?;  // optional cast, returns '.None' on failure
```

`as` performs safe casts and returns `Option<T>`. Combine with `!` or `?` to handle cast failures explicitly.

## Operator Precedence

Operators bind in this order (highest to lowest):

1. Postfix: `!`, `?`, `.`, `[]`, function calls
2. Prefix: `not`, `addr`, unary `-`
3. Cast: `as`
4. Power: `^`
5. Shift: `shl`, `shr`
6. Multiplicative: `*`, `/`, `mod`
7. Additive: `+`, `-`
8. Range: `..`, `..<`
9. Comparison: `<`, `<=`, `>`, `>=`
10. Equality: `=`, `=/=`, `is`
11. Bitwise AND: `and`
12. Bitwise XOR: `xor`
13. Bitwise OR: `or`
14. Short-circuit AND: `and then`
15. Short-circuit OR: `or else`
16. Assignment: `<-`

Parentheses override precedence. Use them liberally when intent is unclear.
