# Existentialize Your Generics

**Authors:** Dimi Racordon, Matt Bovel, Hamza Remmal
**Affiliation:** EPFL, LAMP -- Lausanne, Switzerland
**Venue:** MPLR '25, October 12–18, 2025, Singapore
**DOI:** <https://doi.org/10.1145/3759426.3760975>

---

## Abstract

The two main approaches to compile generic programs are dynamic dispatch and monomorphization. While monomorphization is typically preferred in low-latency applications (where boxing overhead can be prohibitive), it comes at the cost of modularity, expressiveness, and code size.

The Swift programming language proposes an interesting third alternative: supporting dynamic dispatch without boxing by factoring method tables out of object headers. This paper examines the merits of that strategy -- called **existentialization** -- across different programming languages. Our study shows that existentialization can produce code with competitive performance relative to monomorphization.

**CCS Concepts:** Software and its engineering → Object oriented languages; Polymorphism; Classes and objects; Source code generation.

**Keywords:** Existentialization, Generic Programming, Dynamic Dispatch, Type Classes, Separate Compilation

---

## 1. Introduction

Generic programming is the discipline of lifting general abstractions from concrete implementations. Two main strategies exist in mainstream languages:

1. **Dynamic dispatch** -- defers function selection to runtime.
2. **Monomorphization** -- creates specialized copies of polymorphic functions/data structures by substituting concrete types for generic parameters.

Languages targeting low-latency applications typically use monomorphization, since it compiles as efficiently as non-generic code. However, it has three important limitations:

1. **Separate compilation:** A polymorphic function may be recompiled at each call site, hindering binary compatibility.
2. **Non-termination:** Recursive calls nesting type arguments in structural types cannot be monomorphized (would produce unbounded copies).
3. **Code blowup:** The same definitions may be copied many times into compiled output.

Dynamic dispatch avoids these limitations but introduces a different challenge: if `f` is compiled only once, how should its arguments be represented? In object-oriented languages (e.g., Java), values are boxed with object headers providing runtime type info and method dispatch. In systems programming, boxing is discouraged due to indirection overhead and cache unfriendliness.

One workaround is to box values "on demand" into **existential containers** (e.g., `dyn trait` in Rust). However, this weakens type system guarantees (e.g., the binary method problem), may require costly heap allocation, and scales poorly -- transforming a `Vec<Circle>` into `Vec<Box<dyn Shape>>` can turn an O(1) operation into O(n).

**Swift's approach:** When a polymorphic function cannot be monomorphized, it is compiled to a monomorphic version accepting type parameters as additional *term* parameters. The information otherwise stored in object headers is factored out, so generic arguments can be passed as type-erased pointers to unboxed concrete values. This is **existentialization**, a key part of Swift's resilient binary interface strategy.

Swift's compiler currently favors monomorphization when applicable, suggesting existentialization should be used sparingly for performance-critical code. **This paper challenges that assumption**, showing that existentialized code can run as efficiently as monomorphized code on modern VMs, and with competitive performance on bare metal.

### Contributions

- A language-agnostic description of existentialization, identifying key challenges and implementation strategies depending on source language and compilation target.
- Performance measurements comparing existentialization to monomorphization via microbenchmarks on homogeneous data structures with varying levels of abstraction.

---

## 2. Background

### 2.1 Boxed vs. Inline Representations

In managed languages, an object is typically a heap-allocated structure containing fields plus a header with runtime type information (identity hash, GC metadata, klass pointer to class metadata including field layouts, vtable, etc.).

**Example -- Scala (boxed):**

```scala
class Vector2(x: Float, y: Float)
class Circle(origin: Vector2, radius: Float):
  def area() = radius * radius * 3.14f
```

In memory, a `Circle` instance has an object header (mark word + klass pointer), followed by a reference to a separately heap-allocated `Vector2` instance. Reading `origin` requires a pointer indirection.

**Example -- Swift (inline):**

```swift
struct Vector2 {
  var x, y: Float
}
struct Circle {
  var origin: Vector2
  var radius: Float
  func area() -> Float { radius * radius * 3.14 }
}
```

A `Circle` is represented as three contiguous floats -- no header, no indirection. This pairs well with monomorphization, but requires carrying type info across module boundaries when doing separate compilation.

### 2.2 Resilience Boundaries

To support individual component updates, libraries should maintain ABI-compatible interfaces. In Scala (boxed), adding a field to `Circle` is not a breaking change since the class contents are boxed and layout is not part of the ABI. In Swift (inline), the same change *is* a breaking change unless compiled with library evolution support, because it changes the struct size.

To support this in inline-representation languages, clients must not emit code that depends on specific memory layouts -- instead obtaining sizes, alignments, and field offsets at runtime. In Swift, ABI compatibility is described relative to **resilience boundaries** defined in package manifests.

---

## 3. Existentialization

### 3.1 Encoding Type Metadata

The central idea: provide information needed to allocate generic data structures and call their methods *without* access to their implementations at compile-time, and *without* altering the representation of concrete instances.

Starting from a boxed representation, we factor information out of object headers. Object headers contain two kinds of data:

1. **Instance-specific** (e.g., mark word: identity hash, GC metadata)
2. **Type-specific** (e.g., klass pointer: field offsets, methods)

Existentialization focuses on the type-specific part.

**Memory Layout.** Type metadata must encode size and alignment. If the language supports direct field access, it should also describe the memory layout of stored members (mirroring class contents). Alternatively, getters/setters can be generated, trading speed for memory.

**Runtime-Sized Types.** Types with sizes determined only at runtime (e.g., C VLAs) require no special consideration -- allocating any generic type already requires runtime-sized allocation in the compilation target. Stack allocation is possible on systems with low-level stack pointer instructions; otherwise, use fixed-size buffers with heap fallback.

**Primitive Operations.** Operations universally supported for all types (assignment, copy, deinitialization) must be dynamically dispatched in generic contexts.

**Example -- type metadata in C++:**

```cpp
template<size_t n>
void trivialcopy(void* d, void* s) {
  memcpy(d, s, n);
}
void trivialdrop(void*) {};

struct Type {
  size_t size;
  vector<Type const*> members;
  void(*copy)(void*, void*);
  void(*drop)(void*);
};

const Type Float{
  4, {},
  &trivialcopy<4>, &trivialdrop
};
const Type Vector2{
  8, {&Float, &Float},
  &trivialcopy<8>, &trivialdrop
};
```

### 3.2 Using Type Metadata

Existentializing a generic function means replacing type parameters with term parameters accepting type metadata, then replacing type-dependent operations with calls through that metadata.

**Example -- polymorphic identity in Swift:**

```swift
func id<T>(_ x: T) -> T { x }
```

**Existentialized form in C++:**

```cpp
void id(void* r, void* x, Type const* T) {
  T->copy(r, x);
}
```

The result is written to a caller-provided pointer because the size of `T` is unknown at compile-time. A call site `id(4.2)` desugars to:

```cpp
float x = 4.2;
float y;
id(&y, &x, &Float);
```

This satisfies resilient ABI requirements: the body of `id` can be modified without changing call sites, and it makes no assumptions about argument memory layout or copy semantics.

### 3.3 Generating Type Metadata

Static metadata (global variables) is not always sufficient -- the set of all instantiated types cannot be enumerated ahead of time in general.

**Example -- generic swap in C++:**

```cpp
template<typename T, typename U>
pair<U, T> swap(pair<T, U> const& p) {
  return make_pair(p.second, p.first);
}
```

The existentialized form of `swap` requires metadata for `pair<U, T>`, constructed at runtime from `T` and `U`. A hash table can intern metadata instances for a one-to-one type–metadata mapping, enabling fast type equality and curbing memory overhead.

This also solves a key monomorphization limitation: recursive functions nesting type arguments in structural types. The following C++ program cannot compile (monomorphization diverges), but its Java equivalent would:

```cpp
template<typename T>
int f(T const& a, int n) {
  return (n > 3)
    ? 0
    : f(make_pair(a, 1), n + 1);
}
int main() { return f(1, 0); }
```

With existentialization, only one version of `f` is compiled; metadata for each recursive call is computed lazily at runtime.

### 3.4 Dynamic Dispatch

Two approaches for dispatching user-defined methods:

1. **Closed method sets** (e.g., Java): store virtual method tables directly in type metadata.
2. **Open method sets / type classes** (e.g., Swift protocols, Rust traits): use **dictionary-passing**.

**Example -- dictionary-passing in Swift:**

```swift
protocol Shape {
  func area() -> Float
}
func smallest<T: Shape>(x: T, y: T) -> T {
  x.area() > y.area() ? y : x
}
```

The `Shape` protocol compiles to a C++ struct of function pointers:

```cpp
struct Shape {
  float(*area)(void*);
};
```

The existentialized `smallest`:

```cpp
void smallest(
  void* r, void* x, void* y,
  Type const* T, Shape const* shape
) {
  if (shape->area(x) > shape->area(y)) {
    T->copy(r, y);
  } else {
    T->copy(r, x);
  }
}
```

For runtime-sized allocations within existentialized functions, `alloca` can be used for stack allocation:

```cpp
float totalArea(
  void* xs, Type const* T, Shape const* shape
) {
  auto* c = array_is_collection(T);
  auto* e = alloca(T->size);  // stack-allocate element storage
  float s = 0.0f;
  for (size_t i = 0; i < c->count(xs); ++i) {
    c->at(e, xs, i);
    s += shape->area(e);
    shape->drop(e);
  }
  return s;
}
```

> **Note:** In languages enforcing global uniqueness of type class instances (like Swift), witnesses can be constructed directly. In languages supporting local instances (like Scala), witnesses must be passed as additional parameters.

### 3.5 Sharing

Existentialization is compatible with sharing and managed pointers. Shared managed buffers can be implemented with tail-allocated headers (refcount + size), customizing the copy operation to increment the reference counter rather than copying bytes:

```cpp
struct ManagedBuffer {
  static void copy(void* d, void* s) {
    auto* b = static_cast<size_t**>(s);
    auto* h = *b - 2;
    *h += 1;
    memcpy(d, s, sizeof(void*));
  }
  static void drop(void* s) {
    auto* b = static_cast<size_t**>(s);
    auto* h = *b - 2;
    *h -= 1;
    if (*h == 0) { free(h); }
    *b = nullptr;
  }
  static constexpr Type metadata{
    sizeof(void*), {}, &copy, &drop
  };
};
```

Customizable primitive operations enable compiler-defined behaviors in generic contexts without requiring a uniform representation.

### 3.6 Type Reflection for Free

Type metadata can support reflection by allowing inspection of its contents. In particular, it enables **type tests** without type erasure:

```scala
def isTwo[T](v: T) = v match
  case i: Int    => i == 2
  case s: String => s == "two"
  // case o: Option[Int] => o == Some(2)  // fails on JVM due to type erasure
  case _ => false
```

With existentialization, the generic type parameter becomes a term parameter. Type tests reduce to pointer equality on metadata:

```cpp
bool is_two(void* v, Type const* T) {
  if (T == metadata(&Option, &Int)) { ... }
  ...
}
```

Since metadata is interned, pointer equality implies type equality -- solving the type erasure problem.

---

## 4. Evaluation

### 4.1 Benchmarks

Three variants of a generic algorithm that sums the areas of 10 million shapes in a collection:

| Variant                       | Element type          | Collection type               |
| ----------------------------- | --------------------- | ----------------------------- |
| **Monomorphized**             | `Circle` (concrete)   | `Array` (concrete)            |
| **Partially existentialized** | Abstract (`T: Shape`) | `Array` (concrete)            |
| **Fully existentialized**     | Abstract (`T: Shape`) | Abstract (`C: Collection<T>`) |

Types used:

```swift
protocol Shape {
  func area() -> Double
}
struct Vector2 {
  var x, y: Double
}
struct Circle: Shape {
  let center: Vector2
  let radius: Double
  func area() -> Double { radius * radius * 3.14 }
}
```

Languages tested: **Swift** (native existentialization), **C++**, **Rust**, **Scala**, **TypeScript/JavaScript** (last two used to assess JIT optimization impact).

### 4.2 Setup in Swift

Monomorphized variant -- fully specialized by compiler; LLVM IR confirms `area()` is inlined:

```swift
public func area<T: Shape>(
  _ shapes: ContiguousArray<T>
) -> Double {
  var totalArea: Double = 0.0
  var it = shapes.makeIterator()
  while let e = it.next() {
    totalArea += e.area()
  }
  return totalArea
}
```

Partially existentialized -- forced via `@_optimize(none)` forwarder to prevent monomorphization:

```swift
@_optimize(none)
public func areaPartialExist<T: Shape>(
  _ shapes: ContiguousArray<T>
) -> Double { area(shapes) }
```

LLVM IR confirms three witness parameters and dynamic dispatch through a pointer in the Shape witness.

Fully existentialized -- same trick with an additional type parameter for the collection:

```swift
func areaColl<T: Shape, C: Collection<T>>(_ shapes: C) -> Double {
  var totalArea: Double = 0.0
  var it = shapes.makeIterator()
  while let e = it.next() { totalArea += e.area() }
  return totalArea
}

@_optimize(none)
func areaCollEx<T: Shape, C: Collection<T>>(_ shapes: C) -> Double {
  areaColl(shapes)
}
```

Compiled LLVM IR signature:

```llvm
define double @"areaShape"(
  ptr noalias %0, ptr %T, ptr %C,
  ptr %T.Shape, ptr %C.Collection
) { ... }
```

### 4.3 Setup in C++, Rust, Scala, TypeScript

Existentialization is emulated manually by passing type metadata and type class instances explicitly. The C++ implementation mirrors Swift's compiler output and satisfies the same resilience requirements.

**Type metadata struct:**

```cpp
struct Type {
  std::size_t size, alignment;
  void(*copy)(void*, void*);
  void(*drop)(void*);
};
```

**Type class witness for `Iterator` protocol:**

```cpp
struct IteratorWitness {
  Type const* element_type;
  bool(*has_next)(void*);
  void*(*next)(void*);
};
```

**Monomorphized benchmark:**

```cpp
template<typename T>
double area(Vector<T> const& shapes) {
  double total_area = 0.0;
  auto i = VectorIterator<T>{&shapes, 0};
  while (i.has_next()) {
    auto const* s = i.next();
    total_area += (*s)->area();
  }
  return total_area;
}
```

**Partially existentialized:**

```cpp
template<typename T>
double area_partially_existentialized(
  Vector<T> const& shapes,
  Type const* t_type,
  ShapeWitness const* t_shape
) {
  double total_area = 0.0;
  auto* w = VectorIteratorWitness(t_type);
  auto* i = alloca(w->element_type);
  w->make_iterator(i, &shapes);
  while (w->has_next(i)) {
    auto* s = w->next(i);
    total_area += t_shape->area(s);
  }
  return total_area;
}
```

**Fully existentialized:**

```cpp
template<typename T>
double area_fully_existentialized(
  void* shapes,
  IterableWitness const* t_iterable,
  ShapeWitness const* t_element_shape
) {
  double total_area = 0.0;
  auto* w = t_iterable->iterator_iterator;
  auto* i = alloca(t_iterable->iterator_type->size);
  t_iterable->make_iterator(i, shapes);
  while (w->has_next(i)) {
    auto* s = w->next(i);
    total_area += t_element_shape->area(s);
  }
  t_iterable->iterator_type->drop(i);
  return total_area;
}
```

> Artifact: <https://github.com/mbovel/generics-benchmarks>

### 4.4 Performance Measurements

Results normalized relative to C++ monomorphized baseline. Benchmarks: 50 warm-up + 50 measured iterations on Linux Intel (i7-2600 @ 3.4GHz), Mac Intel (i7 2.6GHz), and Mac ARM (M1 Pro).

**Compiler/runtime versions:** rustc 1.89.0-nightly, clang 20.1.2, TypeScript via ts-node 10.9.2 on Node.js 23.6.0, Scala 3.7.0 on OpenJDK 21, Swift 6.1.2.

**Key observations:**

- Monomorphized versions are faster in most cases. C++ and Rust show ~2–2.5× overhead for existentialized variants.
- **Swift fully existentialized** performs significantly worse (>30× overhead) due to heap allocation of iterators and return value copies. Iterator returns owned copies rather than references, costing two extra dynamic calls. Profiling shows >50% of time in `nanov2_malloc_type`/`nanov2_free`. A manual reimplementation using borrowed references achieved ~100ms (vs. >650ms).
- **ARM platforms:** Partially existentialized is as fast as monomorphized in Rust and C++, likely due to the `BL` (branch with link) instruction avoiding stack saves.
- **x86 Rust:** Fully existentialized is slightly faster than partially existentialized (unexplained; confirmed not due to fewer dynamic calls).
- **JIT languages (Scala, TypeScript):** JIT optimizations effectively eliminate existentialization overhead, making it a viable alternative to subtyping.

---

## 5. Conclusion

We studied existentialization -- a technique originating in Swift for separate compilation of generic functions and data structures. Unlike traditional dynamic dispatch, existentialization avoids boxing, supporting efficient inline data representations. Type information (sizes, method tables) is passed separately, reminiscent of dictionary-passing in type-class systems.

We applied existentialization manually across C++, Rust, Scala, TypeScript, and Swift to evaluate performance. Key findings:

- Monomorphization still produces the fastest bare-metal code.
- Existentialization yields **competitive performance** in most cases.
- Swift's implementation shows significant slowdowns due to heap allocation and unnecessary copying -- addressable via borrowed references or improved compiler optimization of iteration patterns.
- JIT-compiled languages (Scala, TypeScript) fully eliminate existentialization overhead, making it a strong alternative to subtyping for generic compilation.

---

## Acknowledgments

Funded by the Swiss National Science Foundation project "Capabilities for Typing Resources and Effects" (TMAG-2_209506/1).

---

## References

1. Bruce et al. (1995). On Binary Methods. *Theory Pract. Object Syst.* 1(3), 221–242.
2. Chakravarty et al. (2005). Associated types with class. *POPL 2005*.
3. Chen, R. (2024). Inside STL: The string. <https://devblogs.microsoft.com/oldnewthing/20230803-00/?p=108532>
4. Drossopoulou et al. (1998). What is Java Binary Compatibility? *OOPSLA 1998*.
5. Gregor, D. (2024). Swift for C++ Practitioners, Part 4: Generics. <https://www.douggregor.net/posts/swift-for-cxx-practitioners-generics/>
6. Iyer et al. (2024). Automatically Reasoning About How Systems Code Uses the CPU Cache. *OSDI 2024*.
7. Lutze et al. (2025). The Simple Essence of Monomorphization. *Proc. ACM Program. Lang.* 9(OOPSLA1), 1015–1041. doi:10.1145/3720472
8. Musser & Stepanov (1988). Generic Programming. *ISSAC*.
9. Pestov, S. (2024). Compiling Swift Generics: Implementation Reference Manual. Unpublished.
10. Racordon et al. (2024). Existential Containers in Scala. *MPLR 2024*.
11. Racordon et al. (2025). On the State of Coherence in the Land of Type Classes. *Art Sci. Eng. Program.* 10(1). doi:10.22152/PROGRAMMING-JOURNAL.ORG/2025/10/15
12. Rose & Cohen (2024). Library Evolution for Stable ABIs. <https://github.com/swiftlang/swift-evolution/blob/main/proposals/0260-library-evolution.md>
13. Wadler & Blott (1989). How to Make ad-hoc Polymorphism Less ad-hoc. *POPL 1989*.
14. Wagner et al. (2024). Realistic Realizability: Specifying ABIs You Can Count On. *Proc. ACM Program. Lang.* 8(OOPSLA2), 1249–1278.
15. Wonnacott, D. (2001). Using Accessory Functions to Generalize Dynamic Dispatch. *COOTS 2001*.

---

*Received 2025-06-24; accepted 2025-07-28*
