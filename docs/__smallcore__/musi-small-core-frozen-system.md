# Musi Small-Core Frozen System

## Set-in-Stone Header

- Set-in-stone track: `docs/__smallcore__/PLAN.md`
- Set-in-stone status: final; freeze decision active as of `2026-05-14`.
- Reconciliation source: `docs/__smallcore__/reconciliation.md`

Roadmap linkage: `docs/__smallcore__/PLAN.md`

This document records the current frozen language system for the stripped-down Musi design. It is written as a set of hard rules, not a brainstorm.

Musi is a small, embeddable systems language for SEAM, the **Stack Effect Abstract Machine**. SEAM is both the VM identity and the bytecode artifact identity, analogous in spirit to Erlang/BEAM and `.beam`, but designed around explicit stack effects, compact artifacts, and lowered source projection.

## 1. Design thesis

Musi is designed around mistakes other languages inherited and did not fully correct.

The core problem Musi resolves:

> Other languages hide consequences. Musi makes consequences visible.

The inherited problems Musi rejects:

- hidden control flow in operators and exception syntax;
- hidden authority through globals, imports, ambient context, or effect handlers;
- hidden mutation through declaration-only mutability;
- hidden cleanup through destructors, finalizers, exceptions, or implicit RAII;
- hidden address stability at native/host boundaries;
- hidden ABI and VM contracts;
- source artifacts that preserve too much authorial structure by default;
- oversized language cores where library concepts become keywords.

Musi's counter-design:

- `mut` makes runtime mutability explicit;
- `known` makes compiler-known phase explicit;
- `pin` makes scoped address stability explicit;
- `defer` makes cleanup registration explicit;
- `yield` makes suspension explicit;
- `Maybe` / `Expect` make absence/failure data-shaped;
- capability objects make authority value-shaped;
- stack effects make VM boundary contracts explicit;
- `.seam` artifacts preserve behavior, not authorial source.

## 2. Audience

Musi is for:

- embeddable runtime authors;
- plugin and scripting hosts;
- systems-tool authors;
- VM and bytecode users;
- people who want typed, explicit, low-footprint scripting;
- projects that want closed-source bytecode artifacts without shipping original source;
- developers who want bytecode-level honesty without writing bytecode.

Musi is not primarily for:

- maximal metaprogramming;
- proof assistants;
- runtime metaobject protocols;
- hidden global dynamic scripting;
- algebraic effect handler languages;
- convenience-first web/application syntax.

## 3. Core rule for keywords

A keyword belongs in Musi only when the concept is part of the language foundation: something user code cannot faithfully implement as a function, type, stdlib helper, attribute, or runtime library.

A keyword may be accepted when it changes:

- evaluation;
- binding;
- stack shape;
- GC roots;
- frame lifetime;
- source lowering;
- verifier state;
- artifact visibility;
- type/value phase;
- address stability;
- cleanup behavior.

Keywords should be short ordinary words, generally 2–6 characters.

## 4. Frozen keyword set

The frozen source keyword set is:

```text
and
as
data
defer
else
erased
export
hidden
if
import
in
known
let
match
mut
not
or
pin
recur
shape
then
unsafe
where
xor
yield
```

Count: 25.

## 5. Rejected keywords and source forms

The following are rejected from the core:

```text
ability
any
ask
async
await
branch
case
catch
comptime
const
effect
elif
errat
erratic
false
fickle
finally
fixed
handle
immov
jump
lazy
opaque
perform
pub
rec
resume
select
solid
some
spawn
stable
static
test
throw
true
unwind
var
vol
volatile
volat
when
with
```

The following are also rejected as core syntax:

```text
&&
||
!
&
|
^
~
<<
>>
postfix ?
postfix !
!!
```

`|` is retained only as the alternative marker.

## 6. Everything is an expression

Everything in Musi is an expression.

A statement is an expression used in statement or top-level position with a mandatory semicolon terminator.

A source file is a sequence of top-level expressions, each ending in `;`.

```musi
import "@std";

export let add(a : Int, b : Int) : Int := (
  a + b
);

let x := mut 0;
x := add(x, 1);
```

Assignment returns `()`.

## 7. Delimiters

Musi delimiters classify the block family.

```text
(...)  computation, grouping, calls, tuples, expression sequencing
{...}  structure, data bodies, shape bodies, record-like structure
[...]  type arguments, indexing, arrays, stack effects
```

Braces are not imperative blocks. Computation uses parentheses.

Good:

```musi
if ready then (
  log.write(.Info, "ready");
  run()
) else (
  log.write(.Info, "not ready");
  fallback
)
```

Structural:

```musi
let Buffer := data {
  let ptr : Ptr[mut Byte];
  let len : Nat;
};
```

## 8. Separators and structural symbols

Symbols carry algebraic meaning.

```text
,   product / tuple / parameter / array list
;   statement terminator / sequence / member terminator / stack split
|   alternative / sum / match arm
=>  maps alternative to result
:=  bind or assign
:   type annotation
.   field/member/variant/namespace edge
@   attribute marker
\   lambda literal
_   wildcard pattern
|>  pipeline sugar
??  Maybe fallback sugar
```

Do not overload structural symbols:

```text
:=
=>
|
|>
:
.
@
\
;
,
```

They are grammar, not user operators.

## 9. Blocks and semicolons

Top-level expressions require `;`.

Structural members require `;` when they are record/product members.

Computation blocks:

```musi
(
  expr1;
  expr2;
  finalExpr
)
```

Rules:

- every non-final expression requires `;`;
- the final expression may omit `;`;
- if the final expression has `;`, the block result is `()`.

Examples:

```musi
(
  let x := 1;
  x + 1
)
```

Result: `Int`.

```musi
(
  let x := 1;
  x + 1;
)
```

Result: `()`.

Match arms do not use semicolon terminators. The `|` marker separates arms.

## 10. Binding

`let` binds values.

```musi
let x := 1;
let y : Int := 2;
```

Function declarations use `:` for result type.

```musi
let add(a : Int, b : Int) : Int := (
  a + b
);
```

`->` is function type syntax only.

```musi
let f : (Int, Int) -> Int := \(a : Int, b : Int) => a + b;
```

There is no source `~>` in the small core. Suspension is explicit through `yield` and coroutine/task protocols.

## 11. Lambda literals

Lambda/function literal syntax uses backslash.

```musi
\() => result
\(x : T) => x
\(a : Int, b : Int) => a + b
```

No `fn` keyword is needed.

## 12. Mutability

Immutability is the default.

`mut` is local and applies to the thing immediately to its right.

`mut` in type position describes mutability.

`mut` in value position constructs mutable value/place.

A mutable outer value requires value-position `mut`, even if the type annotation says `mut`.

Valid:

```musi
let x := mut 0;
let x : mut Int := mut 0;
```

Invalid:

```musi
let x : mut Int := 0;
```

The type annotation alone does not silently construct mutability.

Assignment requires a mutable place.

```musi
let x := mut 0;
x := 1;
```

Invalid:

```musi
let y := 0;
y := 1;
```

### Pointer mutability

```musi
Ptr[T]
mut Ptr[T]
Ptr[mut T]
mut Ptr[mut T]
```

Meanings:

| Type             | Pointer value mutable? | Pointee mutable? |
| ---------------- | ---------------------: | ---------------: |
| `Ptr[T]`         |                     no |               no |
| `mut Ptr[T]`     |                    yes |               no |
| `Ptr[mut T]`     |                     no |              yes |
| `mut Ptr[mut T]` |                    yes |              yes |

Example:

```musi
let p : Ptr[mut Byte] := getMutablePtr[mut Byte]();
```

`p` itself is immutable. The pointee is mutable.

Valid:

```musi
p.[0] := 1;
```

Invalid:

```musi
p := otherPtr;
```

Both mutable:

```musi
let p : mut Ptr[mut Byte] := mut getMutablePtr[mut Byte]();
```

## 13. `known`

`known` means compiler-known / compile-time-known.

```musi
known let pageSize := 4096;
```

`known` is not:

- immutable by itself;
- static storage;
- fixed address;
- pinned;
- `const`;
- `comptime`.

It describes phase: the compiler knows the value.

## 14. `pin`

`pin` creates scoped address stability.

```musi
pin buffer as ptr in (
  nativeWrite(ptr, buffer.len)
)
```

`pin` is the operation. `pinned` may exist as a descriptive type/property name, but it is not the keyword.

Pinning preserves mutability of the pinned subject.

```text
T      -> Ptr[T]
mut T  -> Ptr[mut T]
```

`yield` is not allowed inside a `pin` scope in the core model, because suspended frames must not hold scoped pinned addresses.

## 15. `defer`

`defer` registers cleanup for scope exit.

```musi
let file := open(path);
defer close(file);
read(file);
```

Conditional cleanup:

```musi
let keep := mut 0;

let temp := createTemp(path);
defer delete(temp) where not keep;

writeAll(temp, bytes);
rename(temp, finalPath);
keep := 1;
```

The `where` guard is evaluated at scope exit.

`defer` is the source keyword. `unwind` may be used internally as VM/spec terminology, but there is no source `unwind`.

Cleanup runs on normal scope completion and frame abandonment. Cleanup does not run merely because a coroutine suspends with `yield`.

## 16. `yield`

`yield` is the only primitive suspension point.

```musi
let reply := yield request;
```

Meaning:

- suspend the current frame;
- expose `request` to the driver;
- preserve locals, roots, and deferred cleanups;
- resume later with a reply value.

`yield` cannot be implemented as an ordinary function.

Rejected as keywords:

```text
async
await
spawn
```

Scheduling authority belongs to capability objects.

```musi
let handle := scheduler.spawn(work);
let value := task.await();
```

Those are ordinary capability/protocol operations. They are not source keywords.

## 17. `unsafe`

`unsafe` marks a scoped relaxation of safety rules.

```musi
unsafe (
  ptr.[0]
)
```

`unsafe` does not disable all verification. Stack-shape verification remains unless explicitly skipped by accepted compiler attribution.

## 18. Data

`data` defines concrete data.

Record/product data:

```musi
let Buffer := data {
  let ptr : Ptr[mut Byte];
  let len : Nat;
};
```

Sum/variant data:

```musi
let Maybe[T] := data {
| Some(value : T)
| None
};
```

Empty product:

```musi
let UnitLike := data {;};
```

Empty sum:

```musi
let Never := data {|};
```

Do not mix record fields and variant alternatives in one `data` body in the core.

## 19. Type boundary and conformance

The core type boundary is explicit and small:

- `Unit` has exactly one value, written `()`;
- `Empty` has no values;
- `Type` is the type of type-phase type expressions;
- `Unknown` is the opaque top type;
- `Any` is the dynamic type.

Any value can become `Unknown`, but useful operations require narrowing.
`Any` permits dynamic operations with runtime checks.

Conformance uses `|=`:

```musi
T |= Shape
```

Read it as "`T` conforms to `Shape`". It is the audience-facing source spelling for shape conformance.

The word `fits` is reserved for possible plain-language diagnostics or future tooling, but it is not current source syntax.

Type equivalence uses `~=`:

```musi
A ~= B
```

Static or guaranteed cast uses `:>`:

```musi
let a : Any := value :> Any;
```

Runtime type test uses `:?>`:

```musi
let isInt : Bit := value :?> Int;
```

Final source boundary forms:

```text
|=
~=
:>
:?>
```

Internal compiler prose may write subtype relations mathematically. Musi source uses the boundary forms above.

## 20. Maybe and Expect

Canonical absence type:

```musi
let Maybe[T] := data {
| Some(value : T)
| None
};
```

Canonical failure-bearing type:

```musi
let Expect[T, E] := data {
| Success(value : T)
| Failure(error : E)
};
```

The names are intentionally ML-style:

```text
Success of T
Failure of E
```

### Type sugar

```musi
?T
E!T
```

Meanings:

```musi
?T   == Maybe[T]
E!T  == Expect[T, E]
```

Examples:

```musi
let head[T](xs : List[T]) : ?T := ...;
let read(path : Path) : IOError!Bytes := ...;
```

### Maybe fallback

`??` is Maybe fallback only.

```musi
maybe ?? fallback
```

Equivalent:

```musi
match maybe (
| .Some(value) => value
| .None => fallback
)
```

`??` does not apply to `Expect`.

Rejected:

```text
catch
!!
postfix ?
postfix !
```

## 21. Bit values

`Bit` is the primitive one-bit scalar.

By type context:

```text
0 = false
1 = true
```

There are no `true` or `false` keywords.

Conditions and guards require `Bit`.

No truthiness.

Invalid if `count : Int`:

```musi
if count then a else b
```

Valid:

```musi
if count /= 0 then a else b
```

## 22. Conditional selection: `if then else`

`if then else` is the Bit/proposition selection expression.

```musi
if condition then yes else no
```

`else` is mandatory.

No statement-only `if` exists.

Nested `else if` is just nested `if` in the else branch.

```musi
if x < min then min
else if x > max then max
else x
```

There is no `elif` keyword.

## 23. Structural matching: `match`

`match` inspects a value structurally.

```musi
match value (
| pattern where guard => result
| pattern => result
)
```

`where` refines a match arm.

Examples:

```musi
match maybe (
| .Some(x) => x
| .None => fallback
)
```

```musi
match result (
| .Success(bytes) => bytes
| .Failure(error) => log(error)
)
```

Empty match alternatives are valid only for uninhabited subjects:

```musi
match impossible (|)
```

### Pattern forms (frozen)

Pattern forms are:

- wildcard: `_`
- literal pattern
- binder pattern: `name`
- variant destructure: `.Tag(...)`
- record destructure: `{field}` or `{field: innerPattern}`
- tuple destructure: `(a, b, ...)`
- array destructure: `[a, b, ...]`

Pattern operators are:

- alias pattern: `pat as name`
- alternative pattern: `left or right`

Precedence:

- `as` binds tighter than `or`
- `or` chains left-to-right

Named variant payload destructuring uses `name := pattern`.

Trailing commas are accepted in tuple/array/variant/record pattern forms where list syntax appears.

Casts and type tests do not use `as`; they use `:>` and `:?>`.

## 24. Refutable binding: `let ... else`

`let pattern := expr else fallback;` is refutable forward binding.

```musi
let .Some(x) := maybe else fallback;
rest
```

Meaning:

```musi
match maybe (
| .Some(x) => (
    rest
  )
| _ => fallback
)
```

The success path binds and continues. The else path exits the surrounding block with its result.

Example:

```musi
let valueOrZero(m : ?Int) : Int := (
  let .Some(x) := m else 0;
  x
);
```

With `Expect`:

```musi
let load(path : Path) : IOError!Bytes := (
  let .Success(bytes) := read(path) else .Failure(.ReadFailed);
  .Success(bytes)
);
```

## 25. Operators

Core word operators:

```musi
a and b
a or b
a xor b
not a
```

They are strict value operations. They do not short-circuit.

On `Bit`, they operate on `0` and `1`.

On numeric/word/vector types, they are bitwise/value operations where defined.

Short-circuiting is not core operator behavior.

Use `if`, `match`, or explicit stdlib thunk helpers:

```musi
let ok := andThen(ptr /= 0, \() => ptr.[0] = wanted);
```

With UFCS:

```musi
let ok := (ptr /= 0).andThen(\() => ptr.[0] = wanted);
```

Shifts and rotates are named functions/intrinsics, not keywords or symbols.

```musi
word.shiftLeft(3)
word.shiftRight(3)
word.rotateLeft(8)
word.rotateRight(8)
```

No arbitrary symbolic operator definitions are allowed.

Operator overloading is allowed only for a fixed operator set through shapes/protocols.

Fixed overloadable set:

```text
+ - * / %
= /= < <= > >=
and or xor not
```

## 26. UFCS / UDNS

Musi supports universal function call syntax / universal dot notation.

```musi
receiver.name(args)
```

May resolve to:

1. real field/member access;
2. a visible receiver-first function;
3. a visible shape operation for the receiver.

No hidden global search.

Examples:

```musi
Word.shiftLeft(value, 3)
value.shiftLeft(3)

write(logger, .Info, "starting")
logger.write(.Info, "starting")
```

Dynamic dispatch is visible through `erased`.

```musi
let reader : erased Reader := ...;
reader.read(buffer)
```

## 27. Pipeline

`|>` is transparent expression-shape sugar.

```musi
x |> f
```

means:

```musi
f(x)
```

```musi
x |> f(a, b)
```

means:

```musi
f(x, a, b)
```

No placeholders are part of the core.

Accepted:

```musi
let parsed :=
  text
  |> trim
  |> split(.NewLine)
  |> map(parseLine);
```

Lowered canonical form may erase the pipeline entirely.

## 28. Shapes and capability objects

`shape` defines static interface/contract shape.

```musi
let Logger := shape {
  let write(level : LogLevel, text : String) : IOError!();
};
```

Capability objects are ordinary values carrying authority.

```musi
let run(log : erased Logger) : IOError!() := (
  log.write(.Info, "starting")
);
```

No logger value, no logging authority.

Capability objects are the default answer for:

- IO;
- logging;
- filesystem access;
- clock/time;
- randomness;
- scheduler access;
- host services;
- sandbox permissions;
- stateful services.

Algebraic effects and ambient abilities are rejected from the core.

## 29. `hidden` and `erased`

`hidden` hides concrete representation or type identity across a boundary.

```musi
export hidden let File := data {
  let fd : Word;
};
```

External users can name `File`, but cannot observe its hidden fields/representation.

`erased` means runtime type identity is erased and a witness/dispatch representation may be carried.

```musi
let reader : erased Reader := ...;
```

`hidden` is static abstraction.

`erased` is explicit runtime erasure cost.

Rejected replacements:

```text
some
any
opaque
```

## 30. Import and export

`import` loads/imports a module or artifact edge.

```musi
import "path/to/file";
```

`export` publishes a binding as public Musi API.

```musi
export let add(a : Word, b : Word) : Word := (
  a + b
);
```

Host/external exposure is attribution, not the same as Musi API export.

## 31. Attributes

Accepted attribute names:

```text
@deprecated
@skip
@layout
@foreign
@link
@target
```

Attribute names and keys use camelCase.

Attributes are compiler/tool/backend attribution. They do not create normal source behavior.

### `@deprecated`

API/tooling lifecycle attribution.

### `@skip`

Explicit skipped compiler/verifier check attribution.

### `@layout`

Memory/storage layout attribution.

### `@foreign`

Cross-Musi boundary attribution.

Rules:

- `@foreign` on a declaration without a body means an imported external implementation.
- `@foreign` with `export` and a body means a Musi implementation exposed across an external boundary.
- `export` without `@foreign` means public Musi API only.
- `@foreign` direction is determined by body presence and `export`; no `mode` key is part of the frozen surface.

### `@link`

Native library/link requirement attribution for foreign bindings.

### `@target`

Target/platform availability predicate attribution.

The core language reserves these attribute names. Domain-specific attribute payload schemas are compiler/tool contracts, not additional source keywords.

Rejected attribute names:

```text
@native
@primitive
@vmOp
@repr
@memory
@trusted
@unchecked
@host
```

## 32. Stack effects

Stack effects are exposed at VM/external boundaries.

Syntax:

```musi
[inputs ; outputs]
```

Examples:

```musi
[;]
[Word ; Bit]
[Word, Word ; Word]
[Ptr[Byte], Nat ; Nat]
```

Rightmost input is top-of-stack.

Normal source functions do not need stack-effect annotation unless crossing a low-level/VM/external boundary.

`[;]` is an empty stack effect.

## 33. Empty forms

Accepted:

```text
()        unit / empty tuple
[,]       empty array literal
[;]       empty stack effect
(|)       empty alternatives in match context only
data {;}  empty product, one inhabitant
data {|}  empty sum, no inhabitants
```

Rejected:

```text
[]
(,)
(;)
```

`[]` is not empty array and not empty type parameters. Empty array is `[,]`. Empty type arguments are omitted.

## 34. First-class citizens

Runtime first-class:

```text
scalar values
functions / closures
records
variants
arrays / slices / buffers
mutable places
modules-as-records
capability objects
erased shape/interface values
coroutine/suspension values driven by yield
```

Compiler/type-phase first-class or compiler-owned:

```text
types
known values
stack effects
layouts
attributes
source maps
SEAM descriptors
hidden/erased abstraction metadata
```

Not runtime-first-class in the core:

```text
runtime Type objects
proofs
laws
metaobjects
reader macros
runtime syntax objects
full continuations
algebraic effect handlers
ambient abilities
exceptions
arbitrary symbolic operators
```

## 35. SEAM artifact and decompilation policy

SEAM means **Stack Effect Abstract Machine**.

`.seam` is the bytecode/executable artifact identity.

The pipeline:

```text
Authored Musi
  -> canonical lowered Musi / SEAM-normalized Musi
  -> SEAM bytecode artifact
```

A `.seam` artifact stores compact executable semantics, not authorial source.

Decompiler output without a source map returns:

- valid canonical lowered Musi;
- minified source where removable whitespace is removed;
- lowered names;
- compiler-generated temporaries;
- expanded matches;
- erased pipelines;
- erased UFCS method style;
- lowered `?T`, `E!T`, `??`, `let-else`, and other sugar;
- mangled private names.

Decompiler output without a source map does not recover:

- original local names;
- original private helper names;
- comments;
- formatting;
- source pipeline shape;
- source method-call style;
- original helper boundaries;
- authorial structure.

Original source recovery requires a source-map sidecar.

```text
program.seam      compact behavior artifact
program.seam.map  optional source/authorship map
```

Design sentence:

> `.seam` is space-first and semantics-first. `.seam.map` is source-first.

## 36. Decompiled name policy

Without a source map, decompiled `.seam` uses the smallest valid names that preserve linking, built-ins, public ABI, and semantic correctness.

Everything else is mangled.

Preserve:

- keywords;
- built-in type names;
- built-in variant names;
- required standard/no-std symbolic names;
- exported public API names;
- external ABI names;
- names needed for semantic correctness.

Mangle:

- locals;
- private functions;
- private data names;
- private field names;
- private variants;
- private module aliases;
- helper boundaries;
- compiler temporaries.

User identifiers beginning with `__` are forbidden. That namespace is reserved for generated lowered/decompiled names.

Generated name style:

```text
__0, __1       temporaries
__a0, __a1     parameters
__f0, __f1     private functions
__t0, __t1     private data/types
__v0, __v1     private variants
__fld0         private fields
__mod0         private modules
```

Example authored Musi:

```musi
let .Success(bytes) := file.read(buffer) else .Failure(.ReadFailed);
bytes |> parse;
```

Decompiled `.seam` without map:

```musi
let __0:=File_read(file,buffer);match __0(|.Success(__1)=>(let __2:=parse(__1);__2)|.Failure(_)=>(.Failure(.ReadFailed)));
```

This is valid lowered Musi, not authorial source.

## 37. Lowering principles

Lowering erases source convenience.

Examples:

```text
receiver dot calls      -> free receiver-first functions
UFCS/UDNS               -> receiver-first calls
pipeline |>             -> nested/free calls
?T                      -> Maybe[T]
E!T                     -> Expect[T, E]
??                      -> match on Maybe
let-else                -> match
method names            -> prefixed/free function names
operators where useful  -> intrinsic/free function calls
local names             -> generated names without source map
```

Receiver method lowering:

```musi
file.read(buffer)
```

may lower to:

```musi
File_read(file, buffer)
```

UFCS:

```musi
word.shiftLeft(3)
```

may lower to:

```musi
Word_shiftLeft(word, 3)
```

Pipeline:

```musi
text |> trim |> split(.NewLine) |> map(parseLine)
```

may lower to:

```musi
let __0:=trim(text);let __1:=split(__0,.NewLine);let __2:=map(__1,parseLine);__2
```

## 38. Canonical formatting notes

Canonical examples use unindented `|` alternatives inside `match` and sum `data` bodies.

```musi
match x (
| pattern => result
| _ => fallback
)
```

```musi
data {
| Variant
| Other(value : T)
}
```

Record fields use semicolon-terminated members.

```musi
data {
  let field : T;
}
```

## 39. Final identity statement

Musi is an embeddable systems language for people who want bytecode-level honesty without writing bytecode.

It is designed from a ledger of inherited mistakes:

- hidden control flow;
- hidden authority;
- hidden mutation;
- hidden cleanup;
- hidden address stability;
- hidden ABI facts;
- hidden source recovery.

Musi keeps ordinary values first-class, keeps compiler facts explicit, lowers source sugar away, and decompiles artifacts to semantics, not authorship.
