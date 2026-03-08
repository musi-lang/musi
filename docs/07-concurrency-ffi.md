# §7 — Concurrency & FFI

## 7.1 Tasks

No OS threads at the language level. Concurrency is cooperative tasks.

```
let t      := spawn fetch(url);   // t : Task of String
let result := await t;            // suspends until complete
```

Both require `Async` effect in the enclosing function. `Task of 'T` is a first-class value.

```
let tasks   := urls.map((url) -> spawn fetch(url));
let results := tasks.map((t)  -> await t);
```

## 7.2 Channels

`Channel` is a stdlib type, not a language construct.

```
let ch := Channel of Int;
spawn ch.send(42);
let v  := await ch.recv();   // suspends if empty
```

## 7.3 Loops via Tail Recursion

No `for`/`while`/`loop` keywords. Tail-recursive calls compile to `inv.tal` — O(1) stack, verifier-guaranteed.

```
let sum := (acc: Int, n: Int) -> (
    acc                   if n = 0
  | sum(acc + n, n - 1)   if _
);
```

## 7.4 extrin — FFI Bindings

```
#[extrin := "C"]
let malloc : Int ~> Ptr over { Unsafe };

#[extrin := ("C", "sqlite3_open")]
let dbOpen : CString * Ptr of Ptr ~> CInt over { Unsafe };

#[extrin := "JS"]
let consoleLog : String ~> () over { IO };
```

First arg = ABI string. Optional second = symbol name (defaults to binding name). All C/native `extrin` bindings require at minimum `Unsafe`.

### C type mappings

| MUSI | C |
|------|---|
| `Ptr` | `void*` |
| `Ptr of 'T` | `T*` |
| `CInt` | `int` |
| `CChar` | `char` |
| `CString` | `char*` (null-terminated — **not** `String`) |
| `CSize` | `size_t` |

`CString` ≠ `String`. `String` is `Array of Rune`, not null-terminated. Conversion requires an explicit call.

## 7.5 intrin — Compiler Intrinsics

```
#[intrin := "size_of"]   let sizeOf    : forall 'T -> Int;
#[intrin := "transmute"] let transmute : forall 'T 'U -> 'T ~> 'U over { Unsafe };
#[intrin := "unreachable"] let unreachable : forall 'T -> 'T;
#[intrin := "pin"]       let pin       : forall 'T -> ref 'T ~> Ptr of 'T over { Manual, Unsafe };
```

## 7.6 Safe Wrapper Pattern

Contain all `Unsafe` in one function, expose a typed `Result`:

```
let openDb := (path: String) ~> Result of Db, SqlError over { IO } -> (
    let rc := dbOpen(path.toCString(), inout ptr);
    .Ok(Db.{ ptr := ptr })    if rc = 0
  | .Err(.SqlError(rc))       if _
);
```

## 7.7 Embedding API (C-compatible)

```c
musi_vm_t*    musi_vm_create(musi_vm_config_t*);
void          musi_vm_destroy(musi_vm_t*);
musi_result_t musi_vm_load(musi_vm_t*, const uint8_t* bytecode, size_t len);
musi_result_t musi_vm_run(musi_vm_t*, musi_fn_id_t entry);
musi_value_t  musi_vm_get_result(musi_vm_t*);
```

The MUSI GC manages its own heap independently of the host runtime's memory manager.
