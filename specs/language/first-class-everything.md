# First-Class Core Values

Status: frozen 0.1.0 host-language baseline

Musi keeps compiler-owned syntax small. A concept should be syntax only when
ordinary values cannot safely express its consequence.

## Values

Functions, data constructors, shapes, modules, and capability objects are
ordinary values where phase and visibility allow them.

```musi
let add := \(a : Int, b : Int) : Int => a + b;
let tools := { add := add };
tools.add(1, 2);
```

## Functions

Anonymous functions start with `\`.

```musi
\() => result
\(x : T) => x
```

`->` is function type syntax. Declarations use `:` for their result type.

## Capability Objects

Authority is carried by ordinary objects and parameters, not ambient context.

```musi
let Logger := shape {
  let write(level : LogLevel, text : String) : IOError!();
};

let run(log : erased Logger) : IOError!() := (
  log.write(.Info, "starting")
);
```

## Types And Data

`data` and `shape` construct type-level values. `hidden` hides representation
across module boundaries; `erased` makes runtime erasure visible.

```musi
export hidden let File := data {
  let fd : Word;
};

let Reader := shape {
  let read(into : mut Buffer) : IOError!Nat;
};
```

## Design Check

Before adding syntax, ask:

- Is the consequence impossible to express with ordinary values?
- Does the spelling make control, authority, mutability, phase, cleanup, pinning,
  erasure, or ABI boundaries visible?
- Can the feature stay in the standard library instead?
