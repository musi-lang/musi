# Phase 8 — Widen Codegen: Records, Choices, Match, Functions

**Crate:** `musi_codegen`, `musi_vm`
**Goal:** User-defined types, pattern matching, user-defined functions.
**Dependencies:** Phase 6 (bindings/arithmetic/control), Phase 7 (sema)

---

## Deliverables

### New Opcodes

```
Opcode +=
  // Object operations
  | new.obj(u16)          // allocate object with type_id, field_count from type table
  | ld.fld(u16)           // pop obj, push field at index
  | st.fld(u16)           // pop value, pop obj, store field at index

  // Tag operations (for choice types)
  | ld.tag                // pop obj, push discriminant (Int)
  | eq.tag(i64)           // pop obj, push (discriminant == immediate)

  // Dup
  | dup                   // duplicate top of stack
```

### Type Table in .mso

New section in .mso container:

```
Type Table:
  count: u32
  entries: [TypeEntry]
    TypeEntry = {
      name_idx: u16,        // symbol table index
      kind: u8,             // 0 = record, 1 = choice
      field_count: u16,
      fields: [FieldEntry]
    }
    FieldEntry = {
      name_idx: u16,        // symbol table index for field name
    }
```

### Record Allocation and Access

```
// record Point { x: Int, y: Int }
// const p := Point.{ x := 1, y := 2 };
ld.imm.i64 1        // value for x
ld.imm.i64 2        // value for y
new.obj <Point_type_id>   // allocates obj with 2 fields, pops 2 values into fields

// p.x
ld.loc <p_slot>
ld.fld 0             // field index 0 = x

// p.y
ld.loc <p_slot>
ld.fld 1             // field index 1 = y
```

### Record Spread (`<..`)

```
// const q := Point.{ <..p, x := 99 };
ld.loc <p_slot>
ld.fld 1             // copy y from p
ld.imm.i64 99        // new x value
// order fields correctly for new.obj
new.obj <Point_type_id>
```

Codegen determines which fields come from spread vs explicit, emits loads for spread fields and direct values for explicit fields, in field-order.

### Choice (Tagged Union)

```
// choice Option { Some(Int) | None }
// const a := Some(42);
ld.imm.i64 0        // discriminant for Some
ld.imm.i64 42       // payload
new.obj <Option_type_id>  // 2 fields: tag + payload

// const b := None;
ld.imm.i64 1        // discriminant for None
ld.imm.unit          // no payload (still need a slot)
new.obj <Option_type_id>
```

Variant index (0-based declaration order) = discriminant value.

### Pattern Match → Sequential Test-and-Branch

```
// match x {
//   case Some(v) => writeln(int_to_string(v));
//   case None => writeln("nothing");
// }

emit(x)                   // push scrutinee
dup
ld.tag                    // get discriminant
ld.imm.i64 0              // Some's tag
eq.i64
br.false(try_none)

// Some(v) arm:
dup
ld.fld 1                  // payload → v
st.loc <v_slot>
drop                      // drop scrutinee copy
emit(arm_body_1)
br(match_end)

try_none:
dup
ld.tag
ld.imm.i64 1              // None's tag
eq.i64
br.false(match_fail)

// None arm:
drop                      // drop scrutinee copy
emit(arm_body_2)
br(match_end)

match_fail:
// runtime error: non-exhaustive match
halt_error

match_end:
```

**Pattern compilation (general):**
- Literal pattern: compare with equality opcode
- Variable pattern: store into local slot
- Wildcard: no test
- Tuple pattern: test length, then recurse on each element
- Nested patterns: recursive test-and-bind

### User-Defined Functions

```
// fn add(a: Int, b: Int): Int = a + b;
// Compiled into function table entry with:
//   param_count = 2, locals include a (slot 0), b (slot 1)
//   Code: ld.loc 0; ld.loc 1; add.i64; ret

// Call: add(3, 4)
ld.imm.i64 3
ld.imm.i64 4
call <add_fn_idx>
```

- Function params become the first N local slots.
- Return value is the top of stack when `ret` executes.

### Lambda (Non-Capturing Only)

```
// const f := fn(x: Int): Int => x + 1;
// Compiled as anonymous function table entry
// f stores the function index as a value

// f(10)
ld.imm.i64 10
call <lambda_fn_idx>
```

For Phase 8, lambdas don't capture — they're just anonymous function table entries. Closures come in Phase 9.

### Recursive Functions

Work naturally: function is in the function table before codegen of its body, so calls to itself resolve to the correct index.

```
fn factorial(n: Int): Int =
  if n <= 1 then 1
  else n * factorial(n - 1);
```

---

## Milestone

1. Define `choice Option { Some(Int) | None }`, match on it, print result.
2. Define `record Point { x: Int, y: Int }`, create, access fields, print.
3. Record spread: `Point.{ <..p, x := 99 }`.
4. Recursive `factorial(10)` → `3628800`.
5. Lambda: `const f := fn(x: Int): Int => x * 2; writeln(int_to_string(f(21)));` → `42`.
6. `cargo test --workspace` passes.
