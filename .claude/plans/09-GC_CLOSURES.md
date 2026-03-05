# Phase 9 — Garbage Collector + Closures

**Crate:** `musi_vm`, `musi_codegen`
**Goal:** Mark-sweep GC for heap objects, closure support for capturing lambdas.
**Dependencies:** Phase 8 (records, choices, match, functions)

---

## Deliverables

### Heap with Object Headers

```
ObjHeader = {
  type_tag: u16,        // type table index
  mark: bool,           // GC mark bit
  field_count: u16,     // number of fields
}

HeapObject = {
  header: ObjHeader,
  fields: [Value],      // inline field array
}

Heap = {
  objects: Vec<Option<HeapObject>>,  // slot-based, None = free
  free_list: Vec<HeapIdx>,
  bytes_allocated: usize,
  gc_threshold: usize,               // trigger GC when exceeded
}

HeapIdx = u32 (newtype)
```

**Value changes:**
```
Value +=
  | ObjRef(HeapIdx)       // reference to heap object

// String becomes heap-allocated:
// StringObj: header + char data
// Value::String removed, replaced by ObjRef to string object
```

### Mark-Sweep GC

**Roots:**
- Operand stack
- All local variables in all call frames
- Global variables (if any)

**Algorithm:**
```
fn gc():
  // Mark phase
  for root in collect_roots():
    mark(root)

  // Sweep phase
  for idx in 0..heap.objects.len():
    if heap.objects[idx] is Some(obj):
      if obj.header.mark:
        obj.header.mark = false    // reset for next cycle
      else:
        heap.objects[idx] = None   // reclaim
        heap.free_list.push(idx)
        heap.bytes_allocated -= obj.size()

fn mark(value: Value):
  if value is ObjRef(idx):
    obj = heap.objects[idx]
    if obj.header.mark: return     // already visited
    obj.header.mark = true
    for field in obj.fields:
      mark(field)                  // recursive trace
```

**GC trigger:**
- After every `new.obj`, check `bytes_allocated > gc_threshold`.
- If triggered, run GC, then double threshold if occupancy > 50%.

**Safety:**
- GC only runs at allocation points (no mid-instruction collection).
- All roots are on the Rust stack or in VM data structures — no hidden roots.

### Closure Codegen

**Capture analysis (in codegen):**
```
fn analyze_captures(lambda_body, enclosing_scope) → Vec<CapturedVar>:
  free_vars = collect_free_variables(lambda_body)
  captures = []
  for var in free_vars:
    if var is defined in enclosing_scope (not global):
      captures.push(CapturedVar { name: var.name, slot: var.local_slot })
  return captures
```

**New opcodes:**
```
Opcode +=
  | new.closure(u16, u8)   // fn_idx, capture_count — pops N captured values, pushes closure obj
  | ld.cap(u16)             // load from capture environment (index into closure's env)
```

**Closure representation:**
```
ClosureObj = {
  header: ObjHeader,       // type_tag = special CLOSURE type
  fn_idx: u16,             // function table index
  env: [Value],            // captured values (copied at creation time)
}
```

**Codegen for capturing lambda:**
```
// const offset := 10;
// const f := fn(x: Int): Int => x + offset;

// At creation site:
ld.loc <offset_slot>       // push captured value
new.closure <fn_idx> 1     // create closure with 1 capture

// In lambda body, "offset" references:
ld.cap 0                   // load from capture env, not ld.loc
```

**Call dispatch update:**
```
fn call_value(callee: Value, args...):
  match callee:
    FnRef(fn_idx) → call as before
    ObjRef(idx) where heap[idx] is ClosureObj →
      push closure.env values as extra locals (before params)
      call closure.fn_idx
```

**Capture semantics:** Copy at creation time (value capture). Musi uses `const` by default, so captured values are immutable. For `var` captures, the value at closure creation time is captured (snapshot semantics).

### Integration: Strings as Heap Objects

- `String` values become `ObjRef` pointing to a string heap object.
- String operations (concat, compare) work through heap indirection.
- String objects are GC-managed like all other objects.

---

## Milestone

1. Loop creating thousands of records → memory stabilizes (GC reclaims).
2. Closure capturing a local → returns correct value after outer scope exits.
3. Nested closures (closure capturing a closure) work correctly.
4. String-heavy program doesn't leak memory.
5. `cargo test --workspace` passes.

### Milestone test program:

```
fn make_adder(n: Int): fn(Int): Int =
  fn(x: Int): Int => x + n;

const add5 := make_adder(5);
writeln(int_to_string(add5(10)));  // 15

// Stress test: create many objects, verify GC runs
var i := 0;
while i < 10000 loop (
  const p := Point.{ x := i, y := i * 2 };
  i <- i + 1;
);
writeln("done");
```
