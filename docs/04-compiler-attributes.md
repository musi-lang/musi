# Compiler Attributes

Attributes in Musi fall into three distinct tiers:

- public language attrs
- compiler-only `@musi.*`
- inert metadata

Attributes are metadata and control surface. They do not replace core language semantics such as startup, typeclass dispatch, handlers, or derivation.

## Argument Model

Public language attrs share one argument model:

- named and positional args are both valid
- named form is canonical in docs
- positional args map by strict parameter order
- duplicate names are invalid
- unknown names are invalid
- attribute payloads are data-only:
  - string/int/rune literals
  - variant literals (`.None`, `.Some(...)`) where args are data-only
  - arrays (`[...]`) of data-only values
  - records (`{ key := value, ... }`) of data-only values

Examples:

```musi
@link(name := "c", symbol := "puts")
@link("c", "puts")
```

## Public Language Attrs

### `@link(name, symbol)`

Attaches native library and symbol metadata to a `foreign let` declaration.

```musi
@link(name := "c", symbol := "puts")
foreign "c" let puts (msg : CString) : Int;
```

Parameter order:

1. `name`
2. `symbol`

- `name`: native library or link target
- `symbol`: native symbol override

If `symbol` is omitted, the Musi binding name is used.

`@link` is only valid on `foreign let` declarations.

### `@when(os, arch, env, abi, vendor, feature...)`

Marks a declaration or module item as conditionally present for a target.

```musi
@when(os := "linux", arch := "x86_64")
foreign let clock_gettime (id : Int, out : CPtr) : Int;
```

To express multiple alternatives, use arrays:

```musi
@when(os := ["linux", "mac"], arch := ["x86_64", "aarch64"])
foreign let clock_gettime (id : Int, out : CPtr) : Int;
```

Parameter order:

1. `os`
2. `arch`
3. `env`
4. `abi`
5. `vendor`
6. `feature...`

Named predicates are canonical.

`@when` is for target selection only. It is not a general compile-time branching system.

### `@repr(kind)`

Selects the representation family for a data declaration that must cross a foreign or layout-sensitive boundary.

```musi
@repr(kind := "c")
let Point := data { x : Int; y : Int };
```

Parameter order:

1. `kind`

`@repr` chooses the representation family. It does not carry alignment or packing controls by itself.

### `@layout(align, pack)`

Applies explicit layout controls to a layout-sensitive declaration.

```musi
@layout(align := 8, pack := 1)
let Header := data { tag : Int; len : Int };
```

Parameter order:

1. `align`
2. `pack`

`@layout` is for alignment and packing only. Representation family remains the job of `@repr`.

### `@diag.allow(...)`

### `@diag.warn(...)`

### `@diag.deny(...)`

### `@diag.expect(...)`

Controls diagnostics by `ms####` code.

```musi
@diag.allow("ms4023") let x := 1;
@diag.warn("ms2502") @musi.lang(name := "Fake") let Fake := data { Fake };
@diag.expect("ms2502") @musi.lang(name := "Fake") let Fake := data { Fake };
```

- `allow` suppresses matching diagnostics
- `warn` demotes matching diagnostics to warnings
- `deny` promotes matching diagnostics to errors
- `expect` suppresses one matching diagnostic occurrence and emits an error if no match is produced

Diagnostic attrs control policy only. They do not alter parsing, typing, lowering, or runtime semantics.

## Compiler-Only `@musi.*`

Compiler-owned attrs are reserved to compiler modules and internal language/runtime definitions.

### `@musi.lang(...)`

Marks a compiler-owned language definition.

```musi
@musi.lang(name := "Bool")
let Bool := data { True | False };
```

### `@musi.intrinsic(...)`

Marks a compiler-known intrinsic bridge between the language and the runtime or host.

```musi
@musi.intrinsic(opcode := "seq.len")
foreign let musi_seq_len (xs : CPtr) : Int;
```

### `@musi.variant_tag(...)`

Attaches compiler-owned variant tagging metadata where the language/runtime boundary requires an exact internal tag contract.

### `@musi.runtime_layout(...)`

Attaches compiler-owned runtime layout metadata where the runtime object model requires exact internal layout control.

### `@musi.codegen(...)`

Attaches compiler-owned lowering or backend control metadata.

`@musi.*` is not part of the public language contract. User code must not rely on it.

## Inert Metadata

Any non-reserved attr outside the public and compiler-only sets is preserved as metadata only:

```musi
@serde.rename("user_id")
@sql.table("users")
let User := data { id : Int };
```

Inert metadata does not affect:

- resolution
- sema
- layout
- codegen
- runtime behavior

It remains available for tooling, reflection, or a future explicit macro system that claims it deliberately.

## What Attrs Are Not

Attrs do not define:

- startup entrypoints
- derivation syntax
- typeclass dispatch
- operator precedence
- handler semantics
- general compile-time branching
