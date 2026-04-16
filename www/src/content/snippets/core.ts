import type { ContentSnippet } from "./types";

export const coreSnippets = [
	{
		id: "first-file",
		language: "musi",
		sourceText: `let answer := 42;
answer;`,
		evidence: {
			path: "docs/what/language/syntax.md",
			line: 3,
		},
	},
	{
		id: "first-function",
		language: "musi",
		sourceText: `let twice (x : Int) : Int := x + x;
twice(21);`,
		evidence: {
			path: "docs/what/language/syntax.md",
			line: 5,
		},
	},
	{
		id: "music-direct",
		language: "bash",
		sourceText: `music check index.ms
music build index.ms
music run index.seam`,
		evidence: {
			path: "README.md",
			line: 131,
		},
	},
	{
		id: "let-binding",
		language: "musi",
		sourceText: `let port := 8080;
port;`,
		evidence: {
			path: "crates/music_syntax/src/parser/tests.rs",
			line: 24,
		},
	},
	{
		id: "mutable-value",
		language: "musi",
		sourceText: `let counter := mut 1;
counter := 2;
counter;`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1212,
		},
	},
	{
		id: "sequence",
		language: "musi",
		sourceText: `(
  let base := 8000;
  base + 80
);`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 57,
		},
	},
	{
		id: "case-port",
		language: "musi",
		sourceText: `let Port := data {
  | Configured(port : Int)
  | Default
};

let port : Port := .Configured(port := 8080);
match port (
| .Configured(port) => port
| .Default => 3000
);`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1586,
		},
	},
	{
		id: "recursive-case",
		language: "musi",
		sourceText: `let rec loop (x : Int) : Int :=
  match x (
  | 0 => 0
  | _ => loop(x - 1)
  );`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1004,
		},
	},
	{
		id: "import-std-root",
		language: "musi",
		sourceText: `let Std := import "@std";
let Option := Std.option;
let Result := Std.result;`,
		evidence: {
			path: "packages/std/index.ms",
			line: 2,
		},
	},
	{
		id: "types-basic",
		language: "musi",
		sourceText: `let port : Int := 8080;
let identityFn[T] (input : T) : T := input;`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 111,
		},
	},
	{
		id: "types-apply",
		language: "musi",
		sourceText: "identityFn[Int](port);",
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 121,
		},
	},
	{
		id: "data-port",
		language: "musi",
		sourceText: `let Port := data {
  | Configured(port : Int)
  | Default
};`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1586,
		},
	},
	{
		id: "data-port-value",
		language: "musi",
		sourceText: "let port : Port := .Configured(port := 8080);",
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1590,
		},
	},
	{
		id: "data-port-case",
		language: "musi",
		sourceText: `match port (
| .Configured\\(value) => value
| .Default => 3000
);`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1591,
		},
	},
	{
		id: "record-array",
		language: "musi",
		sourceText: `let point := { x := 3, y := 4 };
let values := [1, 2, 3];`,
		evidence: {
			path: "crates/music_syntax/src/parser/tests.rs",
			line: 80,
		},
	},
	{
		id: "spread-record-array",
		language: "musi",
		sourceText: `let point3 := { ...point, z := 5 };
let extended := [0, ...values];`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 173,
		},
	},
	{
		id: "operators-literals-basic",
		language: "musi",
		sourceText: `let port := 8080;
let label := "ready";
let next := port + 1;
let same := next = port + 1;
let capped := port <= 9000;
let masked := 1 shl 3;`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 183,
		},
	},
	{
		id: "ranges-basic",
		language: "musi",
		sourceText: `let closed := 0..10;
let halfOpen := 0..<10;
closed;`,
		evidence: {
			path: "crates/music_syntax/src/parser/tests.rs",
			line: 239,
		},
	},
	{
		id: "receiver-method",
		language: "musi",
		sourceText: "let (self : Int).abs () : Int := self;",
		evidence: {
			path: "crates/music_resolve/src/tests.rs",
			line: 118,
		},
	},
	{
		id: "receiver-method-call",
		language: "musi",
		sourceText: `let one := 1;
one.abs();`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 510,
		},
	},
	{
		id: "slice-helpers",
		language: "musi",
		sourceText: `let Slice := import "@std/slice";
Slice.concat[Int]([1], [2, 3]);`,
		evidence: {
			path: "packages/std/slice/index.test.ms",
			line: 8,
		},
	},
	{
		id: "export-import",
		language: "musi",
		sourceText: `export let answer := 42;
let Local := import "./index.ms";
Local.answer;`,
		evidence: {
			path: "crates/music_sema/src/checker/surface_exports.rs",
			line: 607,
		},
	},
	{
		id: "type-inference",
		language: "musi",
		sourceText: `let port : Int := 8080;
let next := port + 1;
next;`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 248,
		},
	},
	{
		id: "effect-console",
		language: "musi",
		sourceText: `let Clock := effect {
  let tick () : Int;
};`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 317,
		},
	},
	{
		id: "request-console",
		language: "musi",
		sourceText: "request Clock.tick();",
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 270,
		},
	},
	{
		id: "handle-console",
		language: "musi",
		sourceText: `handle Clock.tick() using Clock {
  value => value;
  tick(k) => resume 1;
};`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 331,
		},
	},
	{
		id: "using-signature",
		language: "musi",
		sourceText: `let nextTick () : Int using { Clock } :=
  request Clock.tick();`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1444,
		},
	},
	{
		id: "class-eq",
		language: "musi",
		sourceText: `let Eq[T] := class {
  let (=) (a : T, b : T) : Bool;
  law reflexive (x : T) := .True;
};`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 503,
		},
	},
	{
		id: "instance-eq-int",
		language: "musi",
		sourceText: `let eqInt := instance Eq[Int] {
  let (=) (a : Int, b : Int) : Bool := .True;
};`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 506,
		},
	},
	{
		id: "foreign-puts",
		language: "musi",
		sourceText: 'foreign "c" let puts (msg : CString) : Int;',
		evidence: {
			path: "crates/music_syntax/src/parser/tests.rs",
			line: 70,
		},
	},
	{
		id: "attr-link-foreign",
		language: "musi",
		sourceText:
			'@link(name := "c") foreign "c" let puts (msg : CString) : Int;',
		evidence: {
			path: "crates/music_syntax/src/parser/tests.rs",
			line: 78,
		},
	},
	{
		id: "quote-expr",
		language: "musi",
		sourceText: "quote (x + 1);",
		evidence: {
			path: "crates/music_syntax/src/parser/tests.rs",
			line: 76,
		},
	},
	{
		id: "quote-block",
		language: "musi",
		sourceText: `quote {
  x;
};`,
		evidence: {
			path: "crates/music_syntax/src/parser/tests.rs",
			line: 77,
		},
	},
	{
		id: "quote-without-meta",
		language: "musi",
		sourceText: `let addOne (x : Int) : Int := x + 1;
let addTwo (x : Int) : Int := x + 2;`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 363,
		},
	},
	{
		id: "quote-with-meta",
		language: "musi",
		sourceText: `let addTemplate := quote (x + #(delta));
let addOneSyntax := quote (#(x) + 1);
let addTwoSyntax := quote (#(x) + 2);`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 373,
		},
	},
	{
		id: "install-curl",
		language: "bash",
		sourceText:
			"curl -fsSL https://raw.githubusercontent.com/musi-lang/musi/main/install.sh | sh",
		evidence: {
			path: "README.md",
			line: 61,
		},
	},
	{
		id: "install-powershell",
		language: "powershell",
		sourceText:
			'powershell -NoProfile -ExecutionPolicy Bypass -Command "irm https://raw.githubusercontent.com/musi-lang/musi/main/install.ps1 | iex"',
		evidence: {
			path: "README.md",
			line: 67,
		},
	},
	{
		id: "install-cargo",
		language: "bash",
		sourceText: `git clone https://github.com/musi-lang/musi.git
cd musi
cargo install --locked --force --path crates/music
cargo install --locked --force --path crates/musi`,
		evidence: {
			path: "README.md",
			line: 84,
		},
	},
	{
		id: "foundation-import",
		language: "musi",
		sourceText: `let Core := import "musi:core";
Core;`,
		evidence: {
			path: "crates/musi_foundation/src/lib.rs",
			line: 9,
		},
	},
	{
		id: "runtime-import",
		language: "musi",
		sourceText: `let Runtime := import "musi:runtime";
Runtime.envGet("HOME");`,
		evidence: {
			path: "packages/std/env/index.ms",
			line: 2,
		},
	},
	{
		id: "quickstart",
		language: "bash",
		sourceText: `musi init hello
cd hello
musi check
musi test`,
		evidence: {
			path: "crates/musi/src/main.rs",
			line: 1,
		},
	},
	{
		id: "package-commands",
		language: "bash",
		sourceText: `musi run
musi check
musi build
musi test`,
		evidence: {
			path: "vscode-ext/README.md",
			line: 8,
		},
	},
	{
		id: "stdlib-option-import",
		language: "musi",
		sourceText: `Option.some[Int](8080)
  |> Option.unwrapOr[Int](3000);`,
		evidence: {
			path: "packages/std/option/index.ms",
			line: 6,
		},
	},
	{
		id: "stdlib-result-import",
		language: "musi",
		sourceText: `Result.ok[Int, String](8080)
  |> Result.unwrapOr[Int, String](3000);`,
		evidence: {
			path: "packages/std/result/index.ms",
			line: 6,
		},
	},
	{
		id: "stdlib-testing-import",
		language: "musi",
		sourceText: `let Testing := import "@std/testing";

export let test () :=
  Testing.it("adds values", Testing.toBe(1 + 2, 3));`,
		evidence: {
			path: "packages/std/option/index.test.ms",
			line: 4,
		},
	},
	{
		id: "chapter-getting-started",
		language: "bash",
		sourceText: `curl -fsSL https://raw.githubusercontent.com/musi-lang/musi/main/install.sh | sh
music check index.ms
musi init hello`,
		evidence: {
			path: "README.md",
			line: 61,
		},
	},
	{
		id: "chapter-first-program",
		language: "musi",
		sourceText: `let answer := 42;
answer;`,
		evidence: {
			path: "docs/what/language/syntax.md",
			line: 3,
		},
	},
	{
		id: "chapter-values-and-let",
		language: "musi",
		sourceText: `let port := 8080;
let nextPort := port + 1;
nextPort;`,
		evidence: {
			path: "crates/music_syntax/src/parser/tests.rs",
			line: 24,
		},
	},
	{
		id: "chapter-blocks-and-expressions",
		language: "musi",
		sourceText: `(
  let base := 8000;
  let offset := 80;
  base + offset
);`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 523,
		},
	},
	{
		id: "chapter-mutation",
		language: "musi",
		sourceText: `let counter := mut 1;
counter := counter + 1;
counter;`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1212,
		},
	},
	{
		id: "chapter-literals",
		language: "musi",
		sourceText: `let port := 8080;
let label := "ready";
let enabled := .True;
label;`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 547,
		},
	},
	{
		id: "chapter-operators",
		language: "musi",
		sourceText: `let port := 8080;
let next := port + 1;
let same := next = 8081;
let capped := next <= 9000;`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 559,
		},
	},
	{
		id: "chapter-ranges",
		language: "musi",
		sourceText: `let closed := 0..10;
let halfOpen := 0..<10;
halfOpen;`,
		evidence: {
			path: "crates/music_syntax/src/parser/tests.rs",
			line: 239,
		},
	},
	{
		id: "chapter-functions",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;
let positional := render(8080, 0 = 0);
let labeled := render(secure := 0 = 0, port := 8080);
labeled;`,
		evidence: {
			path: "docs/what/language/syntax.md",
			line: 5,
		},
	},
	{
		id: "named-callable-values",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let f := render;
f(secure := 0 = 0, port := 8080);

let g : (Int, Bool) -> Int := render;
g(8080, 0 = 0);

let h : (host : Int, tls : Bool) -> Int := render;
h(host := 8080, tls := 0 = 0);`,
		evidence: {
			path: "docs/what/language/core/functions.md",
			line: 1,
		},
	},
	{
		id: "chapter-calls",
		language: "musi",
		sourceText: `let greet (name : String) : String := name;
let message := greet("Musi");
message;`,
		evidence: {
			path: "docs/what/language/syntax.md",
			line: 5,
		},
	},
	{
		id: "chapter-methods",
		language: "musi",
		sourceText: `let (self : Int).abs () : Int := self;
let one := 1;
one.abs();`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 510,
		},
	},
	{
		id: "chapter-records",
		language: "musi",
		sourceText: `let point := { x := 3, y := 4 };
let moved := { ...point, y := 9 };
moved;`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1192,
		},
	},
	{
		id: "chapter-arrays-and-slices",
		language: "musi",
		sourceText: `let Slice := import "@std/slice";
let values := [1, 2, 3];
Slice.concat[Int](values, [4]);`,
		evidence: {
			path: "packages/std/slice/index.test.ms",
			line: 8,
		},
	},
	{
		id: "chapter-patterns",
		language: "musi",
		sourceText: `let Port := data {
  | Configured(port : Int)
  | Default
};

let port : Port := .Configured(port := 8080);
match port (
| .Configured\\(value) => value
| .Default => 3000
);`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1586,
		},
	},
	{
		id: "chapter-files",
		language: "musi",
		sourceText: `let answer := 42;
answer;`,
		evidence: {
			path: "docs/what/language/syntax.md",
			line: 3,
		},
	},
	{
		id: "chapter-packages",
		language: "bash",
		sourceText: `musi init hello
cd hello
musi check
musi test`,
		evidence: {
			path: "crates/musi/src/main.rs",
			line: 1,
		},
	},
	{
		id: "chapter-imports-and-exports",
		language: "musi",
		sourceText: `export let answer := 42;
let Option := import "@std/option";
let Local := import "./index.ms";
Local.answer;`,
		evidence: {
			path: "crates/music_sema/src/checker/surface_exports.rs",
			line: 607,
		},
	},
	{
		id: "chapter-type-annotations",
		language: "musi",
		sourceText: `let port : Int := 8080;
let twice (x : Int) : Int := x + x;
twice(port);`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 707,
		},
	},
	{
		id: "chapter-type-inference",
		language: "musi",
		sourceText: `let port : Int := 8080;
let next := port + 1;
next;`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 718,
		},
	},
	{
		id: "chapter-generics",
		language: "musi",
		sourceText: `let identityFn[T] (input : T) : T := input;
let port := identityFn[Int](8080);

let tools := {
  identity := identityFn
};

let copiedPort := tools.identity[Int](port);

let Box1[T] := data {
  | Box1(value : T)
};

let Keeps[F : Type -> Type] := class {
  let keep(value : F[Int]) : F[Int];
};

let boxKeeps := instance Keeps[Box1] {
  let keep(value : Box1[Int]) : Box1[Int] := value;
};

copiedPort;`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 729,
		},
	},
	{
		id: "chapter-classes",
		language: "musi",
		sourceText: `let Eq[T] := class {
  let (=) (a : T, b : T) : Bool;
  law reflexive (x : T) := .True;
};`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 503,
		},
	},
	{
		id: "chapter-instances",
		language: "musi",
		sourceText: `let eqInt := instance Eq[Int] {
  let (=) (a : Int, b : Int) : Bool := .True;
};`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 506,
		},
	},
	{
		id: "chapter-laws",
		language: "musi",
		sourceText: `let Vehicle[T] := class {
  let wheels(self : T) : Int;
  law atLeastFourWheels(vehicle : T) := vehicle.wheels() >= 4;
};

let Car := data {
  | Sports
  | Family
};

let carLaw := instance Vehicle[Car] {
  let wheels(self : Car) : Int := 4;
};`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 503,
		},
	},
	{
		id: "chapter-effects",
		language: "musi",
		sourceText: `let Clock := effect {
  let tick () : Int;
};

request Clock.tick();`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 317,
		},
	},
	{
		id: "chapter-using",
		language: "musi",
		sourceText: `let nextTick () : Int using { Clock } :=
  request Clock.tick();`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1444,
		},
	},
	{
		id: "chapter-handlers",
		language: "musi",
		sourceText: `handle Clock.tick() using Clock {
  value => value;
  tick(k) => resume 1;
};`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 331,
		},
	},
	{
		id: "chapter-foundation",
		language: "musi",
		sourceText: `let Core := import "musi:core";
Core;`,
		evidence: {
			path: "crates/musi_foundation/src/lib.rs",
			line: 9,
		},
	},
	{
		id: "chapter-runtime",
		language: "musi",
		sourceText: `let Runtime := import "musi:runtime";
Runtime.envGet("HOME");`,
		evidence: {
			path: "packages/std/env/index.ms",
			line: 2,
		},
	},
	{
		id: "chapter-stdlib",
		language: "musi",
		sourceText: `let Option := import "@std/option";
Option.some[Int](8080)
  |> Option.unwrapOr[Int](3000);`,
		evidence: {
			path: "packages/std/option/index.ms",
			line: 6,
		},
	},
	{
		id: "chapter-attributes",
		language: "musi",
		sourceText: `@known(name := "Bool")
export let Bool := Bool;

@link(name := "c")
foreign "c" let puts (msg : CString) : Int;

@when(os := "linux")
foreign let clock_gettime (id : Int, out : CPtr) : Int;`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 869,
		},
	},
	{
		id: "chapter-foreign",
		language: "musi",
		sourceText: 'foreign "c" let puts (msg : CString) : Int;',
		evidence: {
			path: "crates/music_syntax/src/parser/tests.rs",
			line: 70,
		},
	},
	{
		id: "foreign-safe-wrapper",
		language: "musi",
		sourceText: `foreign "c" let puts (message : CString) : Int;

export let printLine (message : CString) : Int := unsafe {
  puts(message);
};`,
		evidence: {
			path: "docs/what/language/advanced/foreign.md",
			line: 1,
		},
	},
	{
		id: "chapter-unsafe-and-ffi",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

foreign "c" let get_counter () : CPtr;

let counter := unsafe {
  let raw := get_counter();
  Ffi.ptr.cast[Int](raw);
};

let next := unsafe {
  let offset := Ffi.ptr.offset;
  offset[Int](counter, 1);
};`,
		evidence: {
			path: "docs/what/language/advanced/unsafe-and-ffi.md",
			line: 1,
		},
	},
	{
		id: "ffi-c-abi-signatures",
		language: "musi",
		sourceText: `foreign "c" let puts (message : CString) : Int;
foreign "c" let memset (dst : CPtr, byte : Int, count : Int) : CPtr;`,
		evidence: {
			path: "docs/what/language/advanced/unsafe-and-ffi.md",
			line: 1,
		},
	},
	{
		id: "ffi-typed-pointer-view",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

foreign "c" let get_counter () : CPtr;

let counter := unsafe {
  let raw := get_counter();
  Ffi.ptr.cast[Int](raw);
};`,
		evidence: {
			path: "docs/what/language/advanced/unsafe-and-ffi.md",
			line: 1,
		},
	},
	{
		id: "unsafe-safe-wrapper",
		language: "musi",
		sourceText: `foreign "c" let clock () : Int;

export let currentTicks () : Int := unsafe {
  clock();
};`,
		evidence: {
			path: "docs/what/language/advanced/unsafe-and-ffi.md",
			line: 1,
		},
	},
	{
		id: "chapter-quote-and-syntax",
		language: "musi",
		sourceText: `let addTemplate := quote (x + #(delta));
let addOneSyntax := quote (#(x) + 1);
addOneSyntax;`,
		evidence: {
			path: "www/src/content/snippets/core.ts",
			line: 967,
		},
	},
	{
		id: "chapter-comptime",
		language: "musi",
		sourceText: `let add (a : Int, b : Int) : Int := a + b;
export let answer () : Int := comptime add(20, 22);`,
		evidence: {
			path: "crates/music_session/src/tests.rs",
			line: 97,
		},
	},
	{
		id: "comptime-parameter",
		language: "musi",
		sourceText: `let scale (comptime n : Int, x : Int) : Int := x * n;
export let answer () : Int := scale(3, 14);`,
		evidence: {
			path: "crates/music_session/src/tests.rs",
			line: 437,
		},
	},
	{
		id: "comptime-quote-module",
		language: "musi",
		sourceText: `let generated : Syntax := comptime quote {
  export let answer () : Int := 42;
};

comptime generated;`,
		evidence: {
			path: "crates/music_session/src/tests.rs",
			line: 489,
		},
	},
	{
		id: "comptime-safe-effect",
		language: "musi",
		sourceText: `let Clock := effect {
  @comptimeSafe
  let tick () : Int;
};

export let answer () : Int := comptime request Clock.tick();`,
		evidence: {
			path: "crates/music_session/src/tests.rs",
			line: 220,
		},
	},
	{
		id: "chapter-testing",
		language: "musi",
		sourceText: `let Testing := import "@std/testing";

export let test () :=
  Testing.it("adds values", Testing.toBe(1 + 2, 3));`,
		evidence: {
			path: "packages/std/option/index.test.ms",
			line: 4,
		},
	},
	{
		id: "chapter-running-and-tooling",
		language: "bash",
		sourceText: `music check index.ms
musi check
musi test`,
		evidence: {
			path: "vscode-ext/README.md",
			line: 8,
		},
	},
	{
		id: "chapter-tuples-and-unit",
		language: "musi",
		sourceText: `let status := (8080, "ready");
let empty := ();
status;`,
		evidence: {
			path: "docs/what/language/core/tuples-and-unit.md",
			line: 11,
		},
	},
	{
		id: "chapter-lambdas",
		language: "musi",
		sourceText: `let twice := \\(x : Int) : Int => x + x;
twice(21);`,
		evidence: {
			path: "docs/what/language/core/lambdas.md",
			line: 11,
		},
	},
	{
		id: "chapter-indexing-and-fields",
		language: "musi",
		sourceText: `let point := { x := 3, y := 4 };
let values := [10, 20, 30];
let x := point.x;
let first := values.[0];
x + first;`,
		evidence: {
			path: "docs/what/language/data/indexing-and-fields.md",
			line: 11,
		},
	},
	{
		id: "chapter-data-definitions",
		language: "musi",
		sourceText: `let Port := data {
  | Configured(port : Int, secure : Bool)
  | Default
};

let Settings := data {
  port : Int := 3000;
  label : String;
};`,
		evidence: {
			path: "docs/what/language/data/data-definitions.md",
			line: 11,
		},
	},
	{
		id: "chapter-callable-types",
		language: "musi",
		sourceText: `let Pure := Int -> Int;
let Effectful := Int ~> Int;
Pure;`,
		evidence: {
			path: "docs/what/language/types/callable-types.md",
			line: 11,
		},
	},
	{
		id: "chapter-type-tests-and-casts",
		language: "musi",
		sourceText: `let value := 42;
let isInt := value :? Int;
let same := value :?> Int;
same;`,
		evidence: {
			path: "docs/what/language/types/type-tests-and-casts.md",
			line: 11,
		},
	},
	{
		id: "chapter-forall-types",
		language: "musi",
		sourceText: `let identityFn[T] (input : T) : T := input;
let identityType := forall (T : Type) -> T -> T;

identityFn[Int](8080);`,
		evidence: {
			path: "docs/what/language/types/forall-types.md",
			line: 11,
		},
	},
	{
		id: "chapter-dependent-types",
		language: "musi",
		sourceText: `let Vec[T, n : Nat] := data {
  | Nil() -> Vec[T, 0]
  | Cons(head : T, tail : Vec[T, n]) -> Vec[T, n + 1]
};

partial let parsePort(text : String) : Int := 0;`,
		evidence: {
			path: "docs/what/language/types/dependent-types.md",
			line: 11,
		},
	},
	{
		id: "chapter-operator-forms",
		language: "musi",
		sourceText: `infixl 6 (+);

let add := (+);
let total := add(1, 2);
total;`,
		evidence: {
			path: "docs/what/language/advanced/operator-forms.md",
			line: 11,
		},
	},
	{
		id: "chapter-templates-and-splices",
		language: "musi",
		sourceText: `let port := 8080;
let label := \`port $\{port}\`;
label;`,
		evidence: {
			path: "docs/what/language/advanced/templates-and-splices.md",
			line: 11,
		},
	},
] satisfies readonly ContentSnippet[];
