# The Basics

Bindings describe how names connect to values. Musi keeps this story direct with two keywords and two operators, echoing Swift's clarity but making mutation visible every time (see [Declarations](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/declarations/)).

## Declaring Values

```musi
const answer := 42;
const name := "Nova";
```

`const` introduces an immutable name. `:=` signals creation, so you never conflate it with comparison. Swift and TypeScript reuse `=` here (see [Declarations](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/declarations/) and [Variable Declarations](https://www.typescriptlang.org/docs/handbook/variable-declarations.html)), but Musi pulls the operators apart to remove ambiguity.

## Making Things Mutable

```musi
var counter := 0;
counter <- counter + 1;
```

`var` marks intent to mutate. `<-` performs reassignment and appears nowhere else. The extra symbol keeps updates loud, mirroring how Swift requires `var` and how TypeScript encourages `let` for mutable values while warning via linters (see [Declarations](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/declarations/) and [prefer-const](https://eslint.org/docs/latest/rules/prefer-const)).

## Blocks Return Values

```musi
const total := {
  const base := 10;
  const bonus := 5;
  base + bonus  // last expression becomes block result
};
```

Semicolons separate steps rather than terminate them. Leave the closing expression bare to return it; add a trailing semicolon to get `Unit`, Musi's empty value. Swift uses similar rules for closures, though statements tend to dominate; TypeScript treats blocks as statement-only unless you wrap them in IIFEs. Musi chooses expression blocks so you avoid temporary variables in many cases (see [Closures](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/closures/) and [Functions](https://www.typescriptlang.org/docs/handbook/2/functions.html)).

## Pattern-Aware Conditions

```musi
if case .Some(const value) := maybe_result {
  writeln(`Value: ${value}`);
}
```

Conditions can match patterns directly. This idea nods to Swift's `if let` and TypeScript's user-defined type guards, yet Musi unifies the syntax across `if`, `while`, and `for`. We will explore the full pattern system soon, but for now note that patterns always bind with `const`; make names mutable afterwards if needed (see [Control Flow](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/controlflow/) and [Narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)).

## Try It Out

1. Rewrite a Swift snippet that flips a Boolean using `var flag = true; flag.toggle()` into Musi using `<-`.
2. Translate a TypeScript `if (maybe !== undefined)` check into an `if case` expression.

## Recap

- `const` + `:=` define immutable names; `var` + `<-` handle updates.
- Blocks behave as expressions, returning final values unless you add a trailing semicolon.
- Pattern matching appears in everyday control flow, echoing familiar constructs from Swift and TypeScript but with consistent syntax.

Read on to [Basic Operators](basic-operators.md) for Musi's symbol guide.

### Further Reading

- [Swift Programming Language – Declarations](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/declarations/)
- [TypeScript Handbook – Variable Declarations](https://www.typescriptlang.org/docs/handbook/variable-declarations.html)
- [ESLint – prefer-const Rule](https://eslint.org/docs/latest/rules/prefer-const)
- [Swift Programming Language – Closures](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/closures/)
- [TypeScript Handbook – Functions](https://www.typescriptlang.org/docs/handbook/2/functions.html)
- [Swift Programming Language – Control Flow](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/controlflow/)
- [TypeScript Handbook – Narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)
