# Import, Export, And External Boundaries

Status: frozen 0.1.0 host-language baseline

Musi uses `import` and `export` for module visibility. Native or host ABI boundaries use `@foreign` metadata on ordinary declarations. There is no `native`, `extern`, `pub`, or `foreign` source keyword in the small core.

## Exports

Top-level declarations are private by default. `export` adds a binding to the public Musi API.

```musi
export let add(a : Int, b : Int) : Int := (
  a + b
);
```

`export hidden let` exports a type name while hiding its representation.

```musi
export hidden let File := data {
  let fd : Word;
};
```

## Imports

`import` brings in a Musi source export record. Bound imports are ordinary values.

```musi
let io := import "@std/io";
```

`as` is not import alias syntax. It is pattern alias syntax (`pat as name`). Casts and type tests use `:>` and `:?>`.

## External Boundaries

`@foreign + declaration without body` imports an external implementation. `@foreign + export + body` exports an external entry point. Direction is visible from `export` and body presence; do not duplicate direction with `mode`, `import`, or `export` keys.

```musi
@foreign(
  name := "musi_read",
  abi := .c,
  stack := [Word, Ptr[Byte], Nat ; Nat]
)
let readFd(fd : Word, ptr : Ptr[Byte], len : Nat) : Nat;
```

Exact `@foreign` body keys beyond non-direction facts are not canonicalized here.
