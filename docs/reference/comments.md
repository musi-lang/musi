# Comments

Musi keeps ordinary comments, item documentation comments, and module documentation
comments separate.

## Ordinary Comments

Use `--` for a line comment:

```musi
-- local explanation
let value := 1;
```

Use `/- ... -/` for a block comment:

```musi
/-
multi-line explanation
-/
let value := 1;
```

## Item Docs

Use `---` for a line documentation comment attached to the following item:

```musi
--- value shown in examples
let value := 1;
```

Use `/-- ... -/` for a block documentation comment attached to the following
item:

```musi
/-- value shown in examples -/
let value := 1;
```

## Module Docs

Use `--!` for a line documentation comment attached to the current module:

```musi
--! Math helpers used by examples.

export let one := 1;
```

Use `/-! ... -/` for a block documentation comment attached to the current
module:

```musi
/-!
Math helpers used by examples.
-/

export let one := 1;
```

Module docs stay separate from the first declaration. Tooling extracts them for
module hover and import documentation.
