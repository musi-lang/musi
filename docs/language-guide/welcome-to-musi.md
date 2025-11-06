# Welcome to Musi

Musi is a systems language that aims for readable code, predictable memory, and a welcoming learning path. It keeps surface syntax simple enough while leaving low-level control within reach.

## Your First Program

```musi
const greeting := "Hello, Musi!";
writeln(greeting);
```

This tiny script introduces Musi's two assignment forms: `const` for stable names and `:=` as declaration marker. Names stay explicit so you always see creation and mutation as separate steps.

## Mindset and Goals

- Use value semantics by default so data stays predictable.
- Reach for optional and expect types instead of nulls. Optional values keep absence honest, while expect values record success or failure out in the open.
- Prefer expressions that return values, meaning blocks often end with useful results instead of plain statements.

## Familiar Building Blocks

- Immutable names stay loud with `const name := "Ada";`
- Mutable updates call out state changes with `var count := 0; count <- 1;`
- Optional checks lean on pattern matching: `if const value := maybe { ... }`

## What Comes Next

Continue to [A Musi Tour](a-musi-tour.md) for a fast walk through core features before diving into detailed chapters.
