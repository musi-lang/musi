# Welcome to Musi

Musi is a systems language that aims for readable code, predictable memory, and a welcoming learning path. Think of it as Swift's clarity meeting TypeScript's gradual typing while keeping low-level control within reach.

## Your First Program

```musi
const greeting := "Hello, Musi!";
writeln(greeting);
```

This tiny script introduces Musi's two assignment forms: `const` for stable names and `:=` as declaration marker. Musi borrows Swift's strong emphasis on immutability ([Declarations](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/declarations/)) while keeping TypeScript's friendly string handling ([Everyday Types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html)).

## Mindset and Goals

- Use value semantics by default so data stays predictable.
- Reach for optional and expect types instead of nulls, similar to Swift optionals ([Optionals](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/thebasics/#Optionals)) and TypeScript union types ([Union Types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#union-types)).
- Prefer expressions that return values, meaning blocks often end with useful results instead of plain statements.

## Familiar Parallels

| Idea | Musi | Swift | TypeScript |
|------|------|-------|------------|
| Immutable name | `const name := "Ada";` | `let name = "Ada"` | `const name = "Ada"` |
| Mutable update | `var count := 0; count <- 1;` | `var count = 0; count = 1` | `let count = 0; count = 1` |
| Optional check | `if const value := maybe { ... }` | `if let value = maybe { ... }` | `if (value !== undefined) { ... }` |

## What Comes Next

Continue to [A Musi Tour](a-musi-tour.md) for a fast walk through core features before diving into detailed chapters.

### Further Reading

- [Swift Programming Language – Declarations](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/declarations/)
- [TypeScript Handbook – Everyday Types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html)
- [Swift Programming Language – Optionals](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/thebasics/#Optionals)
- [TypeScript Handbook – Union Types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#union-types)
