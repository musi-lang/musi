# SEAM External Boundaries, Artifacts, Archives, Decompilation, and Source Maps

## Set-in-Stone Header

- Set-in-stone track: `docs/__smallcore__/PLAN.md`
- Set-in-stone status: frozen 0.1.0 baseline active as of `2026-05-14`.
- Reconciliation source: `docs/__smallcore__/reconciliation.md`

Status: normative freeze document (0.1.0 baseline).

Covers:

```text
8. external boundary and ABI contracts
9. decompiler/minifier/source-map format
```

This document defines how SEAM crosses the host boundary, how `.seam` and `.mar` artifacts differ, how debug/release and thin/fat profiles work, and how decompilation/source maps preserve or discard authorship.

## Tooling names

```text
music  compiler binary, low-profile javac/rustc-like tool
musi   runtime/package/user-facing binary, Bun/Deno/Cargo-like workflow tool
SEAM   VM and bytecode system
.seam  compiled SEAM module
.mar   Musi archive/package
```

Canonical inspection commands:

```text
disasm   bytecode/mnemonic view
decomp   canonical lowered Musi projection
```

`decomp` is chosen because it is the natural 6-character counterpart to `disasm`. This does not imply a `comp` command.

## Artifact roles

```text
.ms         authored Musi source
.seam       canonical SEAM bytecode module
.mar        Musi archive/package, analogous to .jar
.seam.map   optional module authorship/source map
.mar.map    optional archive authorship/source map
```

No `.seamil` as canonical peer artifact.

Textual bytecode is a disassembly view of `.seam`:

```sh
musi disasm app.seam
```

Canonical lowered Musi projection:

```sh
musi decomp app.seam
musi decomp app.mar
```

## `.seam`

`.seam` is a single compiled SEAM module image.

It contains:

```text
header/version/domain manifest
constant/type/layout/function tables
imports/exports
external descriptors
bytecode bodies
stack-effect tables
root maps
optional debug/source-map reference
```

It is analogous to:

```text
.class
.beam
.pyc
```

but with SEAM’s own stack-effect verifier and descriptors.

## `.mar`

`.mar` is a Musi archive/package, analogous to `.jar`.

A `.mar` may contain module blobs, resources, maps, and manifests, or it may be flattened into archive-wide tables depending on build profile. Its identity is archive/package, not inherently minified or flattened.

A `.mar` may be:

```text
debug thin
debug fat
release thin
release fat
release fat + flattened/shrunk/obfuscated
source-retaining
mapped
stripped
```

The profile decides retention and optimization.

## Profile axes

Use two axes:

```text
profile: debug | release
package: thin | fat
```

### Debug profile

```text
source maps included by default
private/original names preserved where practical
module boundaries preserved
minimal shrinking/obfuscation
good diagnostics and decompilation
larger artifact
```

### Release profile

```text
source maps omitted by default
private names may be mangled
unused private code/resources stripped
module boundaries may be flattened
constants/layouts/functions deduplicated
optimized and smaller artifact
```

### Thin package

```text
archive references dependencies externally
keeps dependency graph separate
smaller package output, more runtime/package resolution
```

### Fat package

```text
archive bundles dependency graph
may flatten dependency modules into package image
better standalone distribution
larger unless release shrinking is applied
```

### Release + fat

This is the ProGuard-like profile:

```text
bundle dependencies
flatten private module boundaries where legal
deduplicate package-wide tables
strip source maps/debug/source payloads by default
mangle private names
strip unused private code/resources
preserve public/export/external/builtin names
```

## `.mar` internal model

A `.mar` may be physically zip-like, table-based, or custom binary archive. The stable conceptual sections are:

```text
archive header
manifest
module table
resource table
constant/type/layout/function tables or module table refs
import/export table
external descriptor table
bytecode body area
root maps
checksums/signature info
optional map/source entries
```

Do not define `.mar` as “a folder of `.seam` files” only. That is one packaging strategy, not the format identity.

## Manifest

Manifest should describe package-level facts:

```text
package name
version
entry export/module
module list or flattened module index
runtime/domain requirements
dependency references
resource index
profile flags
source map pointer if any
```

Manifest syntax is not frozen here. It can be compact binary, table-like, or Musi-like. The important rule is that manifest facts are archive/package facts, not source syntax.

## External boundary

Source-level accepted attribute name:

```musi
@foreign
```

Attribute payload keys are implementation detail. The semantic rule is frozen:

```text
@foreign + declaration without body       imported external implementation
@foreign + export + body                  exposed external entry point
export without @foreign                   public Musi API only
```

No redundant `mode` key is needed for direction. Source shape already provides direction.

## External descriptor

Regardless of source attribute body keys, SEAM external descriptors need to represent:

```text
external name/symbol/handle
ABI/calling convention
argument/result mapping or stack effect
foreign descriptor id
pin/address requirements
nullable/raw pointer rules
domain requirements
ownership/lifetime notes when required
```

Examples of facts, not canonical source keys:

```text
name: "musi_read"
abi: c
stack: [Word, Ptr[Byte], Nat ; Nat]
domain: native
requiresPin: arg1
```

The source spelling can be decided separately. The descriptor payload must be sufficient for verifier/runtime/loader.

## External calls

Bytecode:

```text
call.ffi foreignId
```

Stack effect comes from foreign descriptor:

```text
[args... ; results...]
```

Verifier checks:

```text
argument types match descriptor
raw Ptr arguments are in unsafe/native-allowed context
managed addresses crossing ABI have active pin lease or copy descriptor
nullable values use Maybe/explicit representation
return values satisfy descriptor
```

## Domains

Domain names should stay compact and semantic:

```text
managed
native
link
introspect
```

Meaning:

```text
managed     ordinary managed SEAM runtime operations
native      host/native ABI and raw address boundary
link        import/export/module/archive linkage
introspect  public metadata/reflection allowed by artifact policy
```

Domains are VM/module-contract concepts, not source keywords.

## Interop rules

Recommended frozen interop rules:

```text
Ptr[T] is unsafe, typed, and non-null.
Nullable raw pointer is Maybe[Ptr[T]].
No C-style pointer arithmetic in source.
No array-to-pointer decay.
Safe views Ref[T], MutRef[T], Slice[T] cannot be stored/returned/captured or survive yield.
Managed object stable address requires pin.
Pin is lexical.
```

External descriptors must respect these rules.

`@foreign` direction rules are frozen by body/export shape:

```text
body?   export?   meaning
yes     yes       Musi-defined foreign ABI export
yes     no        invalid
no      yes       foreign import re-exported by this module
no      no        private foreign import
```

## Decompilation layers

There are three different views:

```text
authored Musi
  original user source; only available from source or map

canonical lowered Musi
  valid Musi projection emitted by decomp
  lowered, normalized, expanded, generated names

SEAM disassembly
  bytecode/opcode/mnemonic view emitted by disasm
```

`decomp` output is Musi-shaped. It must not introduce fake source keywords like `call`.

Function calls remain:

```musi
parse(bytes)
```

not:

```musi
call parse(bytes)
```

`call` is a SEAM mnemonic, not Musi source.

## Lowered Musi decompilation

Authored source:

```musi
let .Success(bytes) := file.read(buffer) else .Failure(.ReadFailed);
bytes |> parse;
```

Possible no-map decompilation:

```musi
let __0:=File_read(file,buffer);match __0(|.Success(__1)=>(parse(__1))|.Failure(_)=>(.Failure(.ReadFailed)));
```

Pretty no-map decompilation may add whitespace:

```musi
let __0 := File_read(file, buffer);
match __0 (
| .Success(__1) => (
    parse(__1)
  )
| .Failure(_) => (
    .Failure(.ReadFailed)
  )
);
```

Pretty mode does not recover authorship. It only formats lowered projection.

## What decompilation lowers away

Without source maps, decompilation should not recover:

```text
original local names
comments
original whitespace
pipeline shape
UFCS/UDNS receiver style
let-else sugar
?? sugar
original helper boundaries if flattened
private module structure if flattened
private field/variant names if mangled
```

It may recover:

```text
valid lowered Musi
public/export names
built-in and std/no-std anchors needed for linking
external ABI names
semantic data/variant names when public or built-in
```

## Name mangling policy

User identifiers beginning with `__` are forbidden. Compiler/decompiler generated names use `__`.

Generated name classes:

```text
__0, __1       temporaries
__a0, __a1     parameters
__f0, __f1     private functions
__t0, __t1     private data/types
__v0, __v1     private variants
__fld0         private fields
__mod0         private module records
```

Without maps, private names are mangled as aggressively as possible without breaking:

```text
built-ins
std/no-std symbolic anchors
public exports
external ABI names
layout names required by public descriptors
```

## Name preservation levels

### Always preserve

```text
keywords
built-in primitive type names
canonical built-in/library anchors when required
Maybe / Some / None
Expect / Success / Failure
Bit / Byte / Word / Nat / Int / Ptr / Array / Slice / Ref / MutRef
external symbol strings
```

### Preserve if public/exported

```text
exported function names
exported non-hidden data names
exported non-hidden field/variant names
module public API names
```

### Mangle by default

```text
locals
private functions
private data/type names
private fields
private variants
private module aliases
source-only helper names
compiler temporaries
```

## Source maps

A source map is an authorship sidecar. It can be per module or per archive:

```text
.seam.map
.mar.map
```

It may include:

```text
original file paths
source spans
original identifier names
original module boundaries
comments if intentionally stored
original formatting if intentionally stored
sugar reconstruction data
pipeline/UFCS shape
let-else source form
?? source form
```

Source maps may be stored outside the archive or embedded in debug/source-retaining `.mar` builds.

## Debug/release source-map policy

Debug:

```text
maps included by default
names preserved where possible
module boundaries preserved
```

Release:

```text
maps omitted by default
private names mangled
private module boundaries may flatten
```

Release can still produce maps explicitly if the user asks:

```text
musi build --release --map
```

Exact CLI spelling is not frozen here.

## `disasm`

`disasm` displays SEAM bytecode/mnemonics.

Example:

```text
ld.loc 0
ld.loc 1
call File_read
st.loc 2
ld.loc 2
ld.fld tag
br.z Lfail
ld.loc 2
ld.fld 0
call parse
ret
Lfail:
ld.c ReadFailed
new.obj Failure
ret
```

This output is not Musi source.

## `decomp`

`decomp` displays canonical lowered Musi.

Modes:

```text
compact   no unnecessary whitespace
pretty    formatted lowered Musi, still no source recovery
map       source-projected view using map data
```

`decomp` without maps may be intentionally unpleasant. It is a semantic projection, not an authoring experience.

## Obfuscation and closed-source distribution

Obfuscation/minification is a release/profile behavior, not the essence of `.mar`.

However, the system should support closed-source distribution:

```text
release fat .mar
no maps
private names mangled
private module boundaries flattened
private helpers merged/inlined where legal
```

This is not cryptographic security. It is compact artifact design with authorship separated into optional maps.

Design sentence:

```text
Artifacts reveal enough to run and link. Maps reveal authorship.
```

## Loader behavior

Loading `.seam`:

```text
read header
validate version/features
read tables
verify stack effects
verify domains/imports/exports
prepare root maps
load bytecode bodies
quickening/specialization if enabled
```

Loading `.mar`:

```text
read archive header/manifest
validate checksums/signature if present
load module/package tables
resolve internal module graph
resolve external imports
verify all contained modules or flattened bodies
prepare package root maps/layouts
apply profile/runtime quickening
expose entry/export set
```

## Fat archive flattening

Release-fat flattening may:

```text
merge dependency modules
remove private module boundaries
deduplicate constants
deduplicate layouts
deduplicate identical functions where legal
inline private helpers where legal
strip unused private exports/resources
mangle private names
```

It must not:

```text
rename public exported API names
rename external ABI symbols
change domain requirements silently
change stack effects
change observable source-level behavior
strip resources reachable by manifest/export policy
```

## CLI naming suggestions

User-facing runtime/package command:

```text
musi run
musi build
musi test
musi fmt
musi check
musi disasm
musi decomp
```

Compiler command:

```text
music build
music check
```

Exact command surfaces can evolve, but `disasm` / `decomp` distinction should remain.

## Source Evidence Map (current repo)

This section records where the current implementation surfaces live so
seam-04 decisions stay tied to authored code.

Artifact and descriptor surfaces:

```text
crates/music_seam/src/artifact.rs
crates/music_seam/src/descriptor/
crates/music_seam/src/mar.rs
```

Binary and text transport surfaces:

```text
crates/music_seam/src/binary/encode.rs
crates/music_seam/src/binary/decode.rs
crates/music_seam/src/text/format.rs
crates/music_seam/src/text/parse.rs
crates/music_seam/src/text/builder/
```

CLI disassembly/decompilation entry surfaces:

```text
crates/musi/src/commands/disasm.rs
crates/musi/src/commands/build.rs
crates/musi/src/commands/mod.rs
```

Validation and regression surfaces:

```text
crates/music_seam/src/tests.rs
crates/music_seam/src/assembly_tests.rs
crates/musi/tests/cli.rs
```

## Freeze checklist

```text
[x] `.seam` as only canonical bytecode module artifact
[x] no `.seamil` peer format
[x] `.mar` as stable Musi archive/package, analogous to `.jar`
[x] debug/release profile distinction
[x] thin/fat packaging distinction
[x] release-fat flatten/shrink/mangle policy
[x] source map as only authorship recovery layer
[x] `disasm` vs `decomp` naming
[x] `__` generated-name namespace and user-name ban
[x] `@foreign` direction-by-shape rule
[x] external descriptor required facts
```

## Completion Audit Status

Seam-04 is complete for the current small-core phase.

Verification evidence:

```text
rtk grep "\[ \]" docs/__smallcore__/seam-04-external-artifacts-decomp-mar.md -m 40
-> 0 matches

rtk cargo test -p music_seam --lib
-> pass

rtk cargo test -p music_ir_lower --lib
-> pass
```
