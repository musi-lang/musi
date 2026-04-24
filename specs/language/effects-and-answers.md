# Effects And Answers

Status: proposed

This spec defines algebraic effects as first-class request-domain shapes, first-class answer values, and computing forms that apply answers.

## Model

`effect` constructs a structural set of operations that computations may ask.

```musi
let Console := effect {
  ; let readLine() : String
};
```

`ask` performs an effect request. It marks a computation as effectful unless a surrounding `handle` eliminates or translates the requirement.

```musi
let read : () ~> String := \() : String => (
  ask Console.readLine();
);
```

`answer` constructs a structural value that responds to requests from an effect. Answers are ordinary values.

```musi
let consoleAnswer := answer Console {
  ; let readLine() := (
      resume("input");
    )
};
```

`handle` applies an answer to a computation.

```musi
let readPure : () -> String := \() : String => (
  handle (read();) answer consoleAnswer;
);
```

`resume` uses the suspended continuation capability inside an answer operation.

## Function Types

- `A -> B` is pure callable type.
- `A ~> B` is effectful callable type.

An effectful function may ask unhandled effects. A pure function may not. Handling all requested effects can produce pure function values.

## Delimiters And Members

Effect and answer bodies are structural blocks and use `{ ... }`. Named members use `let`.

Operation bodies compute and use `( ... )`.

## Answers

An answer operation receives request payload and continuation capability. It may:

- resume zero times
- resume once
- resume multiple times when the effect/runtime domain permits it
- return without resuming
- translate the request into another effect or fallible result

Allowed resume multiplicity must be represented in semantic/runtime facts before lowering to SEAM.

## Keyword Boundary

`ask` is a keyword form, not a callable value. Parentheses after keywords belong to that keyword grammar form or are rejected; they never make a keyword into a callee.

Not effect source-model words:

- `request`
- `handler`
- `with`
- `via`

These words are ordinary identifiers unless later grammar gives them source meaning. They are not poison keywords and must not lower to effects by compatibility magic.

## Rust Comparison

Rust 2024 has no built-in algebraic effects. Rust async, generators, `Result`, panics, and trait objects are implementation references only.

Musi exposes effect consequences directly:

- `effect` constructs request-domain shape
- `ask` performs effect request
- `answer` constructs response value
- `handle` applies an answer to a computation
- `resume` exposes continuation use inside answer operations

Do not model Musi examples after Rust `?`, `async`, `await`, trait objects, or panic unwinding.

## Lowering

The compiler may lower effects to continuation passing, stack segments, state machines, direct calls, or runtime opcodes. That is not the source model.
