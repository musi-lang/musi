# Phase 1 -- Shared Infrastructure

**Crate:** `musi_shared`
**Goal:** Foundational types every other crate depends on.
**Dependencies:** None

---

## Deliverables

### Span

A source location descriptor.

```
Span = { start: u32, length: u32 }
Span::DUMMY = Span { start: 0, length: 0 }
```

- `start` is a byte offset into the source file.
- `length` is the byte count of the spanned region.
- `DUMMY` sentinel for synthetic/generated nodes.
- Methods: `end() → u32`, `merge(other) → Span` (union of two spans).

### FileId + SourceDb

File-to-source mapping with line/column lookup.

```
FileId = u32 (newtype)

SourceDb = {
  files: Vec<SourceFile>
}

SourceFile = {
  id: FileId,
  name: String,       // e.g. "examples/hello.ms"
  source: String,      // full file contents
  line_starts: Vec<u32>  // byte offsets of each line start
}
```

- `SourceDb::add(name, source) → FileId` -- registers a file, computes `line_starts`.
- `SourceDb::lookup(file_id, byte_offset) → (line: u32, col: u32)` -- binary search on `line_starts`.
- `SourceDb::source(file_id) → &str` -- retrieve full source text.
- `SourceDb::name(file_id) → &str` -- retrieve file name.
- Line/col are 1-based for display.

### String Interner

Arena-backed string deduplication.

```
Symbol = u32 (newtype)

Interner = {
  map: HashMap<&str, Symbol>,
  strings: Vec<String>,      // indexed by Symbol
}
```

- `Interner::intern(s: &str) → Symbol` -- returns existing symbol or inserts new.
- `Interner::resolve(sym: Symbol) → &str` -- retrieve interned string.
- Same string interned twice → same `Symbol` value (identity test via `==`).
- Pre-intern keywords at construction for fast keyword lookup.

### Diagnostic + DiagnosticBag

Structured error reporting.

```
Severity = Error | Warning | Note

Label = {
  span: Span,
  file_id: FileId,
  message: String,
}

Diagnostic = {
  severity: Severity,
  message: String,
  primary: Label,
  secondary: Vec<Label>,
}

DiagnosticBag = {
  diagnostics: Vec<Diagnostic>,
}
```

- `DiagnosticBag::error(message, span, file_id) → &mut Diagnostic` -- append an error.
- `DiagnosticBag::has_errors() → bool`.
- `DiagnosticBag::iter() → impl Iterator<Item = &Diagnostic>`.
- Rendering: `file:line:col: severity: message` format. Detailed rendering (source underlines, colors) is Phase 12.

### Arena\<T\>

Typed arena returning index handles.

```
Idx<T> = u32 (newtype, phantom-typed)

Arena<T> = {
  items: Vec<T>,
}
```

- `Arena::alloc(value: T) → Idx<T>` -- push and return index.
- `Arena::get(idx: Idx<T>) → &T` -- index into the vec.
- `Arena::get_mut(idx: Idx<T>) → &mut T`.
- Typed index prevents mixing arenas (phantom type parameter on `Idx`).
- No lifetimes -- bootstrap-friendly.

---

## Milestone

1. Intern the same string twice → same `Symbol`.
2. Create a `Span` → look up line/col via `SourceDb`.
3. Build a `Diagnostic` → render to `file:line:col: error: message`.
4. `Arena::alloc` + `Arena::get` round-trips correctly.
5. `cargo test -p musi_shared` passes.
