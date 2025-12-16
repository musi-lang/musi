# Musi Language Specification

## Type System

**Gradual typing** with local Hindley-Milner inference. Unannotated bindings default to `Any`.

### Primitives

- **Integers**: `Int8`, `Int16`, `Int32`, `Int64` (signed)
- **Naturals**: `Nat8`, `Nat16`, `Nat32`, `Nat64` (unsigned)
- **Floats**: `Bin16`, `Bin32`, `Bin64` (IEEE-754 binary floats, hardware primitives)
- **Decimals**: `Dec32`, `Dec64`, `Dec128` (software implementation in STL, uses `Bin*` internally)
- **Character**: `Rune` (Unicode scalar value, UTF-32)
- **Text**: `String` (alias for `[]Rune`)
- **Logic**: `Bool` (true/false)
- **Special**: `Unit` (empty tuple), `Any` (top type), `Never` (bottom type)

### Type Constructors

```ebnf
ty_params = "<", ident, {",", ident}, ">";
ty_args = ty, {",", ty};
ty_app = ident, "<", ty_args, ">";
ty_optional = "?", ty;
ty_array = "[", [aux_lit_number], "]", ty;
ty_ptr = "^", ty;
ty_fn = ty, "->", ty;
ty_tuple = "(", ty, {",", ty}, ")";
ty = ident | ty_app | ty_optional | ty_array | ty_ptr | ty_fn | ty_tuple;
```

**Examples:**

```musi
Int32                    // primitive
List<String>             // generic application
?Int32                   // optional (nullable)
[]String                 // dynamic array
[10]Int32                // fixed-size array
^Int32                   // pointer
(Int32, String) -> Bool  // function type
(Int32, String)          // tuple type
```

### Inference Rules

- **Local scope only**: inference within function bodies, not across boundaries
- **Function parameters**: unannotated params default to `Any`
- **Variable bindings**: infer from initializer or default to `Any`

```musi
val x := 42;              // infers Int32
val y := "hello";         // infers String
val f := fn(a) { a + 1 }; // 'a' infers to Any
```

## Lexical Structure

### Identifiers

```ebnf
aux_ident = letter, {letter | digit | "_"};
aux_escaped_ident = "`", {char}, "`";
ident = aux_ident | aux_escaped_ident;
```

**Examples:** `x`, `myVar`, `_internal`, `` `escaped identifier` ``

### Literals

```ebnf
aux_dec = digit, {digit}, [".", digit, {digit}];
aux_hex = "0x", xdigit, {xdigit};
aux_oct = "0o", odigit, {odigit};
aux_bin = "0b", bdigit, {bdigit};
aux_lit_number = aux_dec | aux_hex | aux_oct | aux_bin;
aux_lit_text = '"', {aux_text_content}, '"';
aux_lit_rune = "'", char, "'";
aux_lit_template_text = "$", '"', {aux_text_content | aux_lit_template_part}, '"';
aux_lit = aux_lit_number | aux_lit_text | aux_lit_template_text | aux_lit_rune | "true" | "false";
```

**Examples:**

```musi
42                // decimal integer
3.14              // decimal float (works for Bin64 or Dec64 depending on context)
0x2A              // hexadecimal
0o52              // octal
0b101010          // binary
"hello"           // string
'c'               // rune
$"Hello {name}"   // template string
true              // boolean
false             // boolean
```

### Operators

```ebnf
aux_bin_op = "+" | "-" | "*" | "/" | "**" | "=" | "/=" | "<" | ">" | "<=" | ">=" | "::" | "??" | "and" | "or" | "&" | "|" | "^" | ".." | "..<";
aux_una_op = "-" | "not" | "~" | "@";
```

### Modifiers

```ebnf
aux_modifiers = {("export" | "extern", [aux_lit_text]) | "unsafe"};
```

- `export`: external visibility
- `extern "lib"`: foreign function declaration
- `unsafe`: manual memory management, no GC

## Attributes

```ebnf
aux_attr = "[<", ident, [aux_attr_args], {",", ident, [aux_attr_args]}, ">]";
aux_attr_args = "(", {(ident | aux_lit), [":=", (ident | aux_lit)], {",", (ident | aux_lit, [":=", (ident | aux_lit)])}}, ")";
```

**Examples:**

```musi
[<inline>]
[<deprecated("use new version")>]
[<link(name := "c", version := "1.0")>]
[<inline, no_mangle>]
```

## Patterns

```ebnf
pat_ident = ident;
pat_lit = aux_lit;
pat_wild = "_";
pat_cons = pat, "::", pat;
pat_tuple = "(", pat, {",", pat}, ")";
pat_array = "[", pat, {",", pat}, "]";
pat_record = ident, ".", "{", aux_pat_fields, "}";
pat_ctor = ident, [ty_args], ["(", [pat], ")"];
pat = pat_ident | pat_lit | pat_wild | pat_cons | pat_tuple | pat_array | pat_record | pat_ctor;
aux_pat_fields = aux_pat_field, {",", aux_pat_field};
aux_pat_field = ident;
```

**Examples:**

```musi
x                // bind to variable
42               // literal match
_                // wildcard
head :: tail     // cons pattern
(x, y)           // tuple destructure
[x, y]           // array destructure
Person.{name}    // record field extract
Some(value)      // sum type constructor match
```

## Expressions

### Auxiliary Definitions

```ebnf
aux_ty_annot = ":", ty;
aux_init = ":=", expr;
aux_param = ["var"], ident, [aux_ty_annot], [aux_init];
aux_param_list = aux_param, {",", aux_param};
aux_params = "(", [aux_param_list], ")";
aux_fn_sig = [ident], [ty_params], [aux_params], [aux_ty_annot];
aux_args = expr, {",", expr};
aux_record_field = ["var"], ident, [aux_ty_annot], [aux_init];
aux_record_fields = aux_record_field, {",", aux_record_field};
aux_sum_case = "case", ident, [ty_args], ["(", [ty_args], ")"];
aux_sum_cases = aux_sum_case, {",", aux_sum_case};
aux_match_case = "case", pat, "=>", expr;
aux_match_cases = aux_match_case, {",", aux_match_case};
```

### Core Expressions

```ebnf
expr_lit = aux_lit;
expr_ident = ident;
expr_field = expr, ".", ident | expr, "#", ident;
expr_index = expr, "[", expr, "]";
expr_call = expr, "(", [aux_args], ")";
expr_deref = expr, ".^";
expr_unary = aux_una_op, expr;
expr_binary = expr, aux_bin_op, expr;
expr_lit_tuple = "(", expr, {",", expr}, ")";
expr_lit_array = "[", expr, {",", expr}, "]";
expr_lit_record = [ident], ".{", {aux_record_field, ","}, "}";
expr_optional_chain = expr, "?", (".", ident | "[", expr, "]" | "(", [aux_args], ")");
expr_block = "{", {stmt}, [expr], "}";
```

### Binding & Functions

```ebnf
expr_bind = [aux_modifiers], ("val" | "var"), ident, [aux_ty_annot], aux_init;
expr_fn = [aux_attr], [aux_modifiers], "fn", aux_fn_sig, expr_block;
```

**Examples:**

```musi
val x: Int32 := 42;
var y := "hello";

fn add(x: Int32, y: Int32): Int32 {
  return x + y;
};

fn identity<T>(x: T): T { x };
```

### Control Flow

```ebnf
expr_if = "if", expr, expr_block, ["else", expr_block];
expr_while = "while", expr, expr_block;
expr_for = "for", ident, "in", expr, expr_block;
expr_match = "match", expr, "{", aux_match_cases, "}";
expr_return = "return", expr;
expr_defer = "defer", expr_block;
expr_break = "break", [expr];
expr_cycle = "cycle";
```

**Examples:**

```musi
if x > 0 { positive() } else { negative() };

while running { process() };

for i in 0..10 { handle(i) };

match opt {
case Some(value) => process(value),
case None => default()
};

return x + 1;
defer cleanup();
break;
cycle;
```

### Type Definitions

```ebnf
expr_record = [aux_attr], [aux_modifiers], "record", ident, [ty_params], "{", aux_record_fields, "}";
expr_sum = [aux_attr], [aux_modifiers], "sum", ident, [ty_params], "{", aux_sum_cases, "}";
```

**Examples:**

```musi
record Point<T> {
  x: T;
  y: T
};

sum Option<T> {
  case Some(T),
  case None
};
```

### Interop

```ebnf
expr_unsafe = "unsafe", expr_block;
expr_import = "import", aux_lit_text;
expr_extern = "extern", [aux_lit_text], "unsafe", "{", {aux_fn_sig, ";"}, "}";
```

**Examples:**

```musi
unsafe {
  val ptr: ^Int32 := malloc(4);
  ptr.^ <- 42;
  free(ptr);
};

import "std/io.ms";

extern "C" unsafe {
  fn malloc(size: Nat64): ^Unit;
  fn free(ptr: ^Unit): Unit;
};
```

### Complete Expression Grammar

```ebnf
expr = expr_lit | expr_ident | expr_field | expr_index | expr_call | expr_deref
     | expr_unary | expr_binary | expr_lit_tuple | expr_lit_array | expr_lit_record
     | expr_optional_chain | expr_block | expr_bind | expr_fn | expr_if | expr_while
     | expr_for | expr_match | expr_return | expr_defer | expr_break | expr_cycle
     | expr_unsafe | expr_import | expr_extern | expr_record | expr_sum;
```

## Statements & Programs

```ebnf
stmt = expr, ";";
prog = {stmt};
```

**Note**: Semicolons are required after statements. Blocks implicitly return their last expression.

## Examples

### Minimal Program

```musi
val io := import "std/io.ms";
io#writeln("Hello, Musi!");
```

### Function Definition

```musi
fn fact(n: Int32): Int32 {
  if n <= 1 { return 1; };
  return n * fact(n - 1);
};
```

### Generic Data Structure

```musi
record List<T> {
  head: T;
  tail: ?^List<T>
};

fn cons<T>(x: T, xs: ?^List<T>): ^List<T> {
  return List.{head := x, tail := xs};
};
```

### Pattern Matching

```musi
sum Expect<T, E> {
  case Ok(T),
  case Err(E)
};

fn unwrap_or<T, E>(result: Expect<T, E>, default: T): T {
  match result {
  case Ok(value) => value,
  case Err(_) => default
  }
};
```

### Unsafe FFI

```musi
extern "C" unsafe {
  fn write(fd: Int32, buf: ^Nat8, count: Nat64): Int64;
};

unsafe {
  val msg := "Hello\n";
  write(1, @msg, 6);
};
```
