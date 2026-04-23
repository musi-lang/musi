# Source Syntax Law

Status: proposed

This spec defines source-facing syntax rules for the first-class-everything surface. It is normative for parser, formatter, diagnostics, docs, and examples.

## Delimiters

Delimiter choice follows semantic role.

- `( ... )` means computing: evaluation, sequencing, selection, control flow, bodies, tuple/product shapes, sequence shapes, and any form that runs or chooses work.
- `{ ... }` means structure: fields, cases as data, operation sets, record values, data definitions, effect shapes, and structural answer values.
- `[ ... ]` means array-shaped collection or type/application index shape where grammar says so.

A construct must not choose braces only because another language uses braces for blocks. A construct must not choose parentheses only because another language uses parentheses for grouping.

Delimiter matrix:

```musi
[a, b];      -- array literal
(a, b);      -- tuple expression
(a; b);      -- sequence expression
{a, b};      -- record literal
T1 + T2;     -- sum type expression
(T1, T2);    -- product type expression
();          -- Unit
(,);         -- empty tuple
(;);         -- empty sequence
x.[a];       -- array index access
x.0;         -- tuple index access
x.bar;       -- namespace/field access
```

Rejected form:

```musi
[a; b];      -- no defined meaning; Musi does not use Rust repeat-array syntax here
```

Type-structure matrix:

```musi
data { a : A; b : B };  -- record typedef: members separated by `;`
data { A | B };         -- sum typedef: variants separated by `|`
data { | A | B | };     -- same sum typedef with leading/trailing case separators
```

Inside `data { ... }`, separator choice distinguishes shape:

- `;` means record/product-like structure.
- `|` means sum/case structure.
- mixed record and sum separators in one `data` body have no defined meaning.

Rust comparison: Rust uses the same comma tuple and array forms for values, but it also assigns meaning to `[expr; len]` repeat arrays and `expr[index]` indexing. Musi keeps `[a; b]` invalid and uses `x.[a]` for array indexing so the array-shaped access edge stays visible.

## Binding, Assignment, And Equality

`:=` is the only bind/assign token.

- Binding introduces a name for a value.
- Assignment writes a value into an existing mutable place.
- The left side and mutability rules decide whether `:=` is binding or assignment.

`=` is equality. `/=` is inequality.

Rejected source operators:

- `==`
- `!=`
- C-family comparison spellings used only because they are familiar elsewhere

`/=` is a single token. It is the ASCII spelling of mathematical not-equal for a language without Unicode identifiers/operators.

## Expressions And Semicolons

Everything that appears in statement position is still an expression.

Top-level and sequence entries are expression statements terminated by mandatory semicolon.
Structural-looking declarations do not get a semicolon exemption.

Correct:

```musi
data Foo { value : Int; };
data Bar { text : String; };
```

Wrong:

```musi
data Foo { value : Int; }
data Bar { text : String; }
```

Formatter, parser diagnostics, docs, and tests must not imply declaration forms can omit semicolon just because they look structural.

## Block Separators

Both imperative/computing blocks and structural blocks allow leading and trailing separators.

Imperative block:

```musi
(; do1; do2; do3;);
```

Structural block:

```musi
data { ; x : Int; y : Int; z : Int; };
```

Structural blocks may also accept leading separators where the separator belongs to the member/case list:

```musi
data {
  ; x : Int := 0
  ; y : Int
  ;
};
```

Sum variants use the same permissive separator rule with `|` as case separator:

```musi
data {
  | One
  | Two(param)
  | Three(param) := alsoValue
  | Four := alsoValue
};
```

Formatter may canonicalize separators, but parser and docs must treat leading and trailing member/case separators as valid when the owning list grammar allows them.

Rust comparison: Rust blocks accept semicolon-terminated statement sequences and can omit some final semicolons in expression position. Musi keeps top-level semicolon mandatory and uses leading/trailing separators in the owning list grammar instead of a block-tail exception.

## Optional And Fallible Tokens

Optional and fallible type sugar:

- `?T` means `Option[T]`.
- `E!T` means `Result[T, E]`.

Compound expression tokens:

- `?.` optional member/index/call chain edge
- `!.` fallible member/index/call chain edge
- `.(...)` operator-member selection edge
- `.[...]` index selection edge
- `??` optional fallback
- `catch` fallible recovery

There is no postfix expression `?` or `!` in the core surface. Force unwrap is a named library operation, not punctuation.

## Chain Operators

Access edges belong to the selector edge, not to the identifier.

Selector forms:

```musi
x.name;      -- named member/message selection
x.[i];       -- index selection
x.(+);       -- operator member selection
x?.name;     -- optional named selection
x?.[i];      -- optional index selection
x?.(+);      -- optional operator selection
x!.name;     -- fallible named selection
x!.[i];      -- fallible index selection
x!.(+);      -- fallible operator selection
```

AST shape should model selector access mode:

- normal
- optional
- fallible

Optional chain edge:

- input base has type `?T`
- missing value short-circuits
- result remains flattened optional

Fallible chain edge:

- input base has type `E!T`
- error short-circuits
- result remains fallible

Mixed optional and fallible chain edges require an explicit bridge such as `??`, `catch`, or a named conversion. The compiler must not silently choose between nested optional/fallible shapes.

`.(op)` selects an operator-named member. It is an access edge like `.[...]`, not a call and not grouped field access.

## Functions, Lambdas, And Function Types

Anonymous functions must start with `\`.

```musi
let f := \(x : Int) : Int => (
  x;
);
```

Rejected:

```musi
let f := (x : Int) : Int => x;
```

`=>` is shared by lambdas and branch arms, but lambdas require `\` to avoid conflict with parameter lists, tuple/product forms, branch arms, and parenthesized expressions.

Function type arrows:

- `A -> B` is pure callable type.
- `A ~> B` is effectful callable type.

`~>` is one token. It must not split into `~` and `>`.

## Operators

Operators are first-class names with fixed grammar precedence.

```musi
let plus := (+);
let selected := Add.(+);
let total := left + right;
```

Infix use desugars to a call of the resolved operator name after parse precedence is fixed. Operator definitions do not define precedence or associativity.

`and`, `or`, `xor`, and `not` are one logical operator family. Operand types select the typed instance:

- `Bool` `and` and `or` are conditional and evaluate the right operand only when needed.
- `Bool` `xor` is eager.
- `Bits[N]` `and`, `or`, `xor`, and `not` evaluate operands eagerly and combine bits pointwise.
- Conditions must have type `Bool`; Musi has no truthiness for integers, bits, unit, strings, arrays, or data values.

Syntax operators are not overloadable value operators:

- `:=`
- `=>`
- `->`
- `~>`
- `:`
- `;`
- `,`
- `.`
- `.[`
- `.(`
- `?.`
- `!.`

## `as` Aliases

`as` aliases an already matched or refined value.

```musi
match value (
  | .Some(x) as whole => whole
);

value :? T as refined;
```

Allowed uses:

- pattern alias: `pattern as name`
- type-test/refinement alias: `expr :? Type as name`

Rejected uses:

```musi
import "./mod" as mod;
let mod := import "./mod" as mod;
```

Import aliasing is ordinary binding:

```musi
let mod := import "./mod";
```

`of` is not a keyword and has no source-model role. It is an ordinary identifier.

## Keyword Boundary

Surface keywords must expose user-visible consequences. Parser policy, elaboration plumbing, registry mechanics, and backend lowering do not deserve source keywords.

Current source-facing consequence words from this design slice:

- `shape`: structural operation/contract shape expression
- `given`: contextual availability/filling
- `answer`: structural response value for effect requests
- `ask`: effect request expression
- `handle`: computing form that applies an answer to a computation
- `resume`: explicit continuation inside an answer operation
- `known`: compile-time availability requirement/capability
- `mut`: mutable place/value capability
- `law`: named proof/property obligation declaration
- `@axiom`: metadata on a bodyless proof binding that marks a named trusted proof root

`let` names values. `law` names obligations. `@axiom` marks trusted proof roots as binding metadata. `data`, `effect`, `shape`, `given`, and `answer` construct expressions/values.

Keywords are never values. Parentheses after a keyword belong to that keyword's grammar form, or are rejected. They never create a call expression whose callee is the keyword.

Structural blocks use `let` for value members and `law` for obligation members:

```musi
let Eq[T] := shape {
  ; let equal(a : T, b : T) : Bool

  ; law reflexive(x : T) := (
      equal(x, x) = .True;
    )
};
```

Trusted proof roots are bodyless proof bindings with `@axiom` metadata:

```musi
@axiom(reason := "trusted external theorem")
let sortedAfterSort[T](xs : List[T])
  : Proof[sorted(sort(xs)) = .True];
```

`@axiom` permits a bodyless `let` proof binding. It creates an ordinary first-class value/function with trust metadata attached to the binding. Anonymous axiom expressions and `proof (...)` keyword forms are not part of the source language.

## Quote And Splice

`quote` constructs hygienic `Syntax` values. The source model follows Racket/Scheme hygienic syntax objects: quoted syntax carries scope and span information, not raw pasted text. Rust's `quote::quote` crate is implementation inspiration only, not Musi's source model.

```musi
let expr := quote (x + 1);

let generated := quote {
  let y := #(expr);
};
```

Splice forms use `#` and are valid only inside `quote`:

- `#name` splices a known syntax value by name
- `#(expr)` evaluates a known-phase expression that returns syntax and splices it
- `#[items]` splices a syntax list/sequence where grammar permits multiple elements

Splicing inserts syntax objects or syntax sequences. It must not degrade to string/token concatenation.

Variants are cases, not named members, and use `|` plus dot access:

```musi
let Bool := data {
  | True
  | False
};

.True;
```

Borrow and pin are runtime and interop concepts, not source syntax promises.

Rejected as source model words:

- `capability`
- `class`
- `instance`
- `request`
- `with`
- `via`
- `using`
- `for`
- `of`
- `provide`

These are ordinary identifiers unless some later grammar gives them source meaning. They are not poisoned reserved words.

## Influence Policy

Musi does not use C, Rust, JavaScript, or TypeScript as default syntax models.

Acceptable references are semantic, not copy-paste syntax: Lean4, Scala3, F#, F*, Haskell, OCaml, Lisp, Ada, Pascal, VHDL, Erlang, Unison, Koka, and Effekt.

Specs and docs must not use fake Musi examples that smuggle in unsupported host-language syntax.
