# Phase 6 — Widen Codegen: Bindings, Arithmetic, Control Flow

**Crate:** `musi_codegen`, `musi_vm`
**Goal:** Extend codegen+VM to handle variables, operators, branching, loops, blocks.
**Dependencies:** Phase 5 (hello world e2e)

---

## Deliverables

### New Opcodes

```
Opcode +=
  // Arithmetic
  | add.i64 | sub.i64 | mul.i64 | div.i64 | mod.i64
  | add.f64 | sub.f64 | mul.f64 | div.f64 | mod.f64
  | neg.i64 | neg.f64

  // Comparison
  | eq.i64 | neq.i64 | lt.i64 | gt.i64 | leq.i64 | geq.i64
  | eq.f64 | neq.f64 | lt.f64 | gt.f64 | leq.f64 | geq.f64
  | eq.bool | neq.bool

  // Logical / bitwise
  | not
  | bit.and | bit.or | bit.xor | bit.not
  | shl | shr

  // Control flow
  | br(i32)               // unconditional relative jump
  | br.true(i32)          // pop, jump if true
  | br.false(i32)         // pop, jump if false

  // String
  | concat.str            // pop two strings, push concatenation
  | eq.str | neq.str
```

### Bindings: `const` / `var`

```
const x := 10;    →  ld.imm.i64 10; st.loc <slot>
var y := 0;        →  ld.imm.i64 0; st.loc <slot>
y <- y + 1;        →  ld.loc <slot>; ld.imm.i64 1; add.i64; st.loc <slot>
```

- Codegen maintains a scope stack mapping names → local slot indices.
- `const` and `var` both use `st.loc` (const-ness is a semantic check, Phase 7).
- Nested scopes: inner scope can shadow outer bindings. Slot is still in the same locals array.

### All Binary Operators

Codegen emits: evaluate LHS, evaluate RHS, emit operator opcode.

**Short-circuit `and`/`or`:**
```
// a and b
emit(a)
br.false(skip_to_end)    // if a is false, result is false
emit(b)
// end:
```

```
// a or b
emit(a)
br.true(skip_to_end)     // if a is true, result is true
emit(b)
// end:
```

### Prefix Operators

```
-x   →  emit(x); neg.i64
not x →  emit(x); not
~x   →  emit(x); bit.not
```

### `if` / `elif` / `else`

```
// if c1 then e1 elif c2 then e2 else e3
emit(c1)
br.false(elif_label)
emit(e1)
br(end_label)
elif_label:
emit(c2)
br.false(else_label)
emit(e2)
br(end_label)
else_label:
emit(e3)
end_label:
```

Without `else`: the `if` produces `Unit` on the false branch.

### `while` / `loop` / `for`

```
// while cond loop body
start_label:
emit(cond)
br.false(end_label)
emit(body)
drop                      // discard body value
br(start_label)
end_label:
ld.imm.unit               // while produces unit
```

```
// loop body
start_label:
emit(body)
drop
br(start_label)
// end_label: (reachable only via break)
```

```
// for pat in iter loop body
// (Phase 6 supports range-based for only)
emit(iter_start)
st.loc <counter>
emit(iter_end)
st.loc <limit>
start_label:
ld.loc <counter>
ld.loc <limit>
lt.i64
br.false(end_label)
// bind pat to counter
emit(body)
drop
ld.loc <counter>
ld.imm.i64 1
add.i64
st.loc <counter>
br(start_label)
end_label:
ld.imm.unit
```

### Blocks

```
// (s1; s2; e)
emit(s1); drop
emit(s2); drop
emit(e)
// block value = last expression
```

### `break` / `cycle`

- `break` → `br(loop_end_label)`, optionally with a value.
- `cycle` → `br(loop_start_label)`.
- With labels: `break 'name` → jump to named loop's end label.
- Codegen maintains a loop context stack: `{ start_label, end_label, label_name? }`.

### Int-to-String Conversion (for printing)

Add intrinsic `int_to_string` (id=3) so we can print numbers:

```
#[intrinsic(int_to_string)] native fn int_to_string(n: Int): String;
```

---

## Milestone

```
const x := 10;
var y := 0;
while y < x loop (
  y <- y + 1;
);
writeln(int_to_string(y));
```
Outputs `10`.

Additional milestone tests:
- Arithmetic: `2 + 3 * 4` → `14`
- Short-circuit: `false and panic()` → no panic
- Nested if/elif/else
- Break with value from loop
- Block as expression
- `cargo test --workspace` passes
