# The Basics

Bindings describe how names connect to values. Musi keeps this story direct with two keywords and two operators so creation and mutation always stand out like a sore thumb.

## Declaring Values

```musi
const answer := 42;
const name := "Nova";
```

`const` introduces an immutable name. `:=` signals creation, so you never conflate it with comparison. By spelling the declaration operator out, we avoid the usual confusion between assignment and equality.

## Making Things Mutable

```musi
var counter := 0;
counter <- counter + 1;
```

`var` marks intent to mutate. `<-` performs reassignment and appears nowhere else. The extra symbol keeps updates loud and clear so refactors cannot miss state changes.

## Blocks Return Values

```musi
const total := {
  const base := 10;
  const bonus := 5;
  base + bonus  // last expression becomes block result
};
```

Semicolons separate steps rather than terminate them. Leave the closing expression bare to return it; add a trailing semicolon to get `Unit`, Musi's empty value. Expression blocks make it easy to pipe computations without temporary variables.

## Pattern-Aware Conditions

```musi
if case .Some(const value) := maybe_result {
  writeln(`Value: ${value}`);
}
```

Conditions can match patterns directly. Musi unifies the syntax across `if`, `while`, and `for`, so you only learn the rules once. We'll explore the full pattern system soon(TM), but for now keep in mind that patterns always bind with `const`; make names mutable afterwards if needed.

## Try It Out

1. Create a Musi snippet that flips a `Bool` flag by rebinding with `<-`.
2. Rewrite a manual optional check into an `if case` expression that binds the value.

## Recap

- `const` + `:=` define immutable names; `var` + `<-` handle updates.
- Blocks behave as expressions, returning final values unless you add a trailing semicolon.
- Pattern matching appears in everyday control flow, keeping optional access and destructuring concise.

Read on to [Basic Operators](basic-operators.md) for Musi's symbol guide.
