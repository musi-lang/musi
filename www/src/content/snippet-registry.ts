export interface SnippetEvidence {
	path: string;
	line: number;
}

export interface ContentSnippet {
	id: string;
	language: string;
	sourceText: string;
	evidence: SnippetEvidence;
}

export const contentSnippets: readonly ContentSnippet[] = [
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
			path: "www/src/content/snippet-registry.ts",
			line: 67,
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
			path: "www/src/content/snippet-registry.ts",
			line: 121,
		},
	},
	{
		id: "types-apply",
		language: "musi",
		sourceText: "identityFn[Int](port);",
		evidence: {
			path: "www/src/content/snippet-registry.ts",
			line: 131,
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
			path: "www/src/content/snippet-registry.ts",
			line: 183,
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
			path: "www/src/content/snippet-registry.ts",
			line: 193,
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
			path: "www/src/content/snippet-registry.ts",
			line: 258,
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
			path: "www/src/content/snippet-registry.ts",
			line: 280,
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
			path: "www/src/content/snippet-registry.ts",
			line: 373,
		},
	},
	{
		id: "quote-with-meta",
		language: "musi",
		sourceText: `let addTemplate := quote (x + #(delta));
let addOneSyntax := quote (#(x) + 1);
let addTwoSyntax := quote (#(x) + 2);`,
		evidence: {
			path: "www/src/content/snippet-registry.ts",
			line: 383,
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
		sourceText: `musi new hello
cd hello
musi run`,
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
musi new hello`,
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
			path: "www/src/content/snippet-registry.ts",
			line: 533,
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
			path: "www/src/content/snippet-registry.ts",
			line: 557,
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
			path: "www/src/content/snippet-registry.ts",
			line: 569,
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
		sourceText: `musi new hello
cd hello
musi run`,
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
			path: "www/src/content/snippet-registry.ts",
			line: 698,
		},
	},
	{
		id: "chapter-type-inference",
		language: "musi",
		sourceText: `let port : Int := 8080;
let next := port + 1;
next;`,
		evidence: {
			path: "www/src/content/snippet-registry.ts",
			line: 709,
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
			path: "www/src/content/snippet-registry.ts",
			line: 720,
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
			path: "www/src/content/snippet-registry.ts",
			line: 832,
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
			path: "www/src/content/snippet-registry.ts",
			line: 857,
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
musi run
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
	{
		id: "c-cpp-values-functions",
		language: "musi",
		sourceText: `let total (basePrice : Int, fee : Int) : Int := basePrice + fee;

let answer := total(1200, 45);
answer;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/values-functions-expressions.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-named-calls",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/values-functions-expressions.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-block-expression",
		language: "musi",
		sourceText: `let invoiceTotal () : Int :=
  (
    let basePrice := 1200;
    let fee := 45;
    basePrice + fee
  );

invoiceTotal();`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-recursive-control-flow",
		language: "musi",
		sourceText: `let rec totalSeats (remaining : Int, seats : Int) : Int :=
  match remaining (
  | 0 => seats
  | _ => totalSeats(remaining - 1, seats + 4)
  );

totalSeats(3, 0);`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-variables-mutation",
		language: "musi",
		sourceText: `let visits := mut 0;
visits := visits + 1;

let nextVisits := visits + 1;
nextVisits;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-fresh-value",
		language: "musi",
		sourceText: `let basePrice := 1200;
let total := basePrice + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-structs-classes-records",
		language: "musi",
		sourceText: `let Endpoint := data {
  host : String;
  port : Int;
  secure : Bool;
};

let local := {
  host := "localhost",
  port := 8080,
  secure := 0 = 1
};

let publicEndpoint := { ...local, host := "api.example.com", secure := 0 = 0 };
publicEndpoint.port;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/structs-classes-records.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-arrays-pointers-slices",
		language: "musi",
		sourceText: `let Iter := import "@std/iter";

let ports := [3000, 8080];
let visible := ports
  |> Iter.append[Int](9000)
  |> Iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/arrays-pointers-slices.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-null-option",
		language: "musi",
		sourceText: `let Option := import "@std/option";

let lookupPort (name : String) : Option.Option[Int] :=
  match name (
  | "admin" => Option.some[Int](9000)
  | _ => Option.none[Int]()
  );

let port := lookupPort("web")
  |> Option.unwrapOr[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-result-value",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsePort (text : String) : Result.Result[Int, String] :=
  match text (
  | "8080" => Result.ok[Int, String](8080)
  | _ => Result.err[Int, String]("invalid port")
  );

let port := parsePort("abc")
  |> Result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-errors-results",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsePort (text : String) : Result.Result[Int, String] :=
  match text (
  | "8080" => Result.ok[Int, String](8080)
  | _ => Result.err[Int, String]("parse error")
  );

let port := parsePort("abc")
  |> Result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/errors-results-effects.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-effect-boundary",
		language: "musi",
		sourceText: `let Io := import "@std/io";

let name := Io.promptTrimmed("name> ");
Io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/errors-results-effects.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-enums-variants-patterns",
		language: "musi",
		sourceText: `let TaskState := data {
  | Waiting
  | Running(id : Int)
  | Done(code : Int)
};

let state : TaskState := .Running(id := 42);
match state (
| .Running(id) => id
| .Waiting => 0
| .Done(code) => code
);`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/enums-variants-patterns.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/templates-concepts-classes-laws.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-concept-class-law",
		language: "musi",
		sourceText: `let Vehicle[T] := class {
  let wheels(self : T) : Int;
  law atLeastFourWheels(vehicle : T) := vehicle.wheels() >= 4;
};

let Car := data {
  | Car
};

let carVehicle := instance Vehicle[Car] {
  let wheels(self : Car) : Int := 4;
};`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/templates-concepts-classes-laws.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-methods-receiver-calls",
		language: "musi",
		sourceText: `let (port : Int).withOffset (offset : Int) : Int := port + offset;

let publicPort := 8080.withOffset(1);
publicPort;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/methods-and-receiver-calls.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/c-cpp/headers-modules-packages.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-module-import",
		language: "musi",
		sourceText: `let Ports := import "./ports";

let port := Ports.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/headers-modules-packages.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-testing-tooling",
		language: "musi",
		sourceText: `let Testing := import "@std/testing";

let suite := Testing.suite("ports", [
  Testing.test("default port", \\() => Testing.expectEqual[Int](8080, 8080))
]);

Testing.run(suite);`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/testing-tooling.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-unsafe-ffi",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

foreign "c" let puts (message : Ffi.CString) : Ffi.CInt;

export let announce (message : Ffi.CString) : Ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/unsafe-ffi-native-boundaries.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-ffi-pointer",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

let pointer := Ffi.ptr.null[Int]();
let samePointer := unsafe { Ffi.ptr.offset[Int](pointer, 0); };
Ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/unsafe-ffi-native-boundaries.md",
			line: 1,
		},
	},
	{
		id: "java-values-methods-expressions",
		language: "musi",
		sourceText: `let total (basePrice : Int, fee : Int) : Int := basePrice + fee;

let answer := total(1200, 45);
answer;`,
		evidence: {
			path: "docs/what/language/developers/java/values-methods-expressions.md",
			line: 1,
		},
	},
	{
		id: "java-named-calls",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/java/values-methods-expressions.md",
			line: 1,
		},
	},
	{
		id: "java-block-expression",
		language: "musi",
		sourceText: `let invoiceTotal () : Int :=
  (
    let basePrice := 1200;
    let fee := 45;
    basePrice + fee
  );

invoiceTotal();`,
		evidence: {
			path: "docs/what/language/developers/java/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "java-recursive-control-flow",
		language: "musi",
		sourceText: `let rec totalSeats (remaining : Int, seats : Int) : Int :=
  match remaining (
  | 0 => seats
  | _ => totalSeats(remaining - 1, seats + 4)
  );

totalSeats(3, 0);`,
		evidence: {
			path: "docs/what/language/developers/java/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "java-variables-mutation",
		language: "musi",
		sourceText: `let visits := mut 0;
visits := visits + 1;

let nextVisits := visits + 1;
nextVisits;`,
		evidence: {
			path: "docs/what/language/developers/java/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "java-fresh-value",
		language: "musi",
		sourceText: `let basePrice := 1200;
let total := basePrice + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/java/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "java-records-classes-objects",
		language: "musi",
		sourceText: `let Endpoint := data {
  host : String;
  port : Int;
  secure : Bool;
};

let local := {
  host := "localhost",
  port := 8080,
  secure := 0 = 1
};

let publicEndpoint := { ...local, host := "api.example.com", secure := 0 = 0 };
publicEndpoint.port;`,
		evidence: {
			path: "docs/what/language/developers/java/records-classes-objects.md",
			line: 1,
		},
	},
	{
		id: "java-collections-streams-pipelines",
		language: "musi",
		sourceText: `let Iter := import "@std/iter";

let ports := [3000, 8080];
let visible := ports
  |> Iter.append[Int](9000)
  |> Iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/java/collections-streams-pipelines.md",
			line: 1,
		},
	},
	{
		id: "java-null-option",
		language: "musi",
		sourceText: `let Option := import "@std/option";

let lookupPort (name : String) : Option.Option[Int] :=
  match name (
  | "admin" => Option.some[Int](9000)
  | _ => Option.none[Int]()
  );

let port := lookupPort("web")
  |> Option.unwrapOr[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/java/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "java-result-value",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsePort (text : String) : Result.Result[Int, String] :=
  match text (
  | "8080" => Result.ok[Int, String](8080)
  | _ => Result.err[Int, String]("invalid port")
  );

let port := parsePort("abc")
  |> Result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/java/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "java-exceptions-results",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsePort (text : String) : Result.Result[Int, String] :=
  match text (
  | "8080" => Result.ok[Int, String](8080)
  | _ => Result.err[Int, String]("parse error")
  );

let port := parsePort("abc")
  |> Result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/java/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "java-effect-boundary",
		language: "musi",
		sourceText: `let Io := import "@std/io";

let name := Io.promptTrimmed("name> ");
Io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/java/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "java-sealed-types-patterns",
		language: "musi",
		sourceText: `let TaskState := data {
  | Waiting
  | Running(id : Int)
  | Done(code : Int)
};

let state : TaskState := .Running(id := 42);
match state (
| .Running(id) => id
| .Waiting => 0
| .Done(code) => code
);`,
		evidence: {
			path: "docs/what/language/developers/java/sealed-types-patterns.md",
			line: 1,
		},
	},
	{
		id: "java-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/java/generics-interfaces-laws.md",
			line: 1,
		},
	},
	{
		id: "java-interface-class-law",
		language: "musi",
		sourceText: `let Vehicle[T] := class {
  let wheels(self : T) : Int;
  law atLeastFourWheels(vehicle : T) := vehicle.wheels() >= 4;
};

let Car := data {
  | Car
};

let carVehicle := instance Vehicle[Car] {
  let wheels(self : Car) : Int := 4;
};`,
		evidence: {
			path: "docs/what/language/developers/java/generics-interfaces-laws.md",
			line: 1,
		},
	},
	{
		id: "java-methods-receiver-calls",
		language: "musi",
		sourceText: `let (port : Int).withOffset (offset : Int) : Int := port + offset;

let publicPort := 8080.withOffset(1);
publicPort;`,
		evidence: {
			path: "docs/what/language/developers/java/methods-and-receiver-calls.md",
			line: 1,
		},
	},
	{
		id: "java-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/java/packages-modules.md",
			line: 1,
		},
	},
	{
		id: "java-module-import",
		language: "musi",
		sourceText: `let Ports := import "./ports";

let port := Ports.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/java/packages-modules.md",
			line: 1,
		},
	},
	{
		id: "java-testing-tooling",
		language: "musi",
		sourceText: `let Testing := import "@std/testing";

let suite := Testing.suite("ports", [
  Testing.test("default port", \\() => Testing.expectEqual[Int](8080, 8080))
]);

Testing.run(suite);`,
		evidence: {
			path: "docs/what/language/developers/java/testing-tooling.md",
			line: 1,
		},
	},
	{
		id: "java-native-unsafe-ffi",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

foreign "c" let puts (message : Ffi.CString) : Ffi.CInt;

export let announce (message : Ffi.CString) : Ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/java/native-unsafe-ffi.md",
			line: 1,
		},
	},
	{
		id: "java-ffi-pointer",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

let pointer := Ffi.ptr.null[Int]();
let samePointer := unsafe { Ffi.ptr.offset[Int](pointer, 0); };
Ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/java/native-unsafe-ffi.md",
			line: 1,
		},
	},
	{
		id: "csharp-values-methods-expressions",
		language: "musi",
		sourceText: `let total (basePrice : Int, fee : Int) : Int := basePrice + fee;

let answer := total(1200, 45);
answer;`,
		evidence: {
			path: "docs/what/language/developers/csharp/values-methods-expressions.md",
			line: 1,
		},
	},
	{
		id: "csharp-named-calls",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/csharp/values-methods-expressions.md",
			line: 1,
		},
	},
	{
		id: "csharp-block-expression",
		language: "musi",
		sourceText: `let invoiceTotal () : Int :=
  (
    let basePrice := 1200;
    let fee := 45;
    basePrice + fee
  );

invoiceTotal();`,
		evidence: {
			path: "docs/what/language/developers/csharp/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "csharp-recursive-control-flow",
		language: "musi",
		sourceText: `let rec totalSeats (remaining : Int, seats : Int) : Int :=
  match remaining (
  | 0 => seats
  | _ => totalSeats(remaining - 1, seats + 4)
  );

totalSeats(3, 0);`,
		evidence: {
			path: "docs/what/language/developers/csharp/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "csharp-variables-mutation",
		language: "musi",
		sourceText: `let visits := mut 0;
visits := visits + 1;

let nextVisits := visits + 1;
nextVisits;`,
		evidence: {
			path: "docs/what/language/developers/csharp/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "csharp-fresh-value",
		language: "musi",
		sourceText: `let basePrice := 1200;
let total := basePrice + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/csharp/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "csharp-records-classes-objects",
		language: "musi",
		sourceText: `let Endpoint := data {
  host : String;
  port : Int;
  secure : Bool;
};

let local := {
  host := "localhost",
  port := 8080,
  secure := 0 = 1
};

let publicEndpoint := { ...local, host := "api.example.com", secure := 0 = 0 };
publicEndpoint.port;`,
		evidence: {
			path: "docs/what/language/developers/csharp/records-classes-objects.md",
			line: 1,
		},
	},
	{
		id: "csharp-collections-linq-pipelines",
		language: "musi",
		sourceText: `let Iter := import "@std/iter";

let ports := [3000, 8080];
let visible := ports
  |> Iter.append[Int](9000)
  |> Iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/csharp/collections-linq-pipelines.md",
			line: 1,
		},
	},
	{
		id: "csharp-null-option",
		language: "musi",
		sourceText: `let Option := import "@std/option";

let lookupPort (name : String) : Option.Option[Int] :=
  match name (
  | "admin" => Option.some[Int](9000)
  | _ => Option.none[Int]()
  );

let port := lookupPort("web")
  |> Option.unwrapOr[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/csharp/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "csharp-result-value",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsePort (text : String) : Result.Result[Int, String] :=
  match text (
  | "8080" => Result.ok[Int, String](8080)
  | _ => Result.err[Int, String]("invalid port")
  );

let port := parsePort("abc")
  |> Result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/csharp/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "csharp-exceptions-results",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsePort (text : String) : Result.Result[Int, String] :=
  match text (
  | "8080" => Result.ok[Int, String](8080)
  | _ => Result.err[Int, String]("format error")
  );

let port := parsePort("abc")
  |> Result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/csharp/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "csharp-effect-boundary",
		language: "musi",
		sourceText: `let Io := import "@std/io";

let name := Io.promptTrimmed("name> ");
Io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/csharp/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "csharp-unions-pattern-matching",
		language: "musi",
		sourceText: `let TaskState := data {
  | Waiting
  | Running(id : Int)
  | Done(code : Int)
};

let state : TaskState := .Running(id := 42);
match state (
| .Running(id) => id
| .Waiting => 0
| .Done(code) => code
);`,
		evidence: {
			path: "docs/what/language/developers/csharp/unions-pattern-matching.md",
			line: 1,
		},
	},
	{
		id: "csharp-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/csharp/generics-interfaces-laws.md",
			line: 1,
		},
	},
	{
		id: "csharp-interface-class-law",
		language: "musi",
		sourceText: `let Vehicle[T] := class {
  let wheels(self : T) : Int;
  law atLeastFourWheels(vehicle : T) := vehicle.wheels() >= 4;
};

let Car := data {
  | Car
};

let carVehicle := instance Vehicle[Car] {
  let wheels(self : Car) : Int := 4;
};`,
		evidence: {
			path: "docs/what/language/developers/csharp/generics-interfaces-laws.md",
			line: 1,
		},
	},
	{
		id: "csharp-extension-methods-and-calls",
		language: "musi",
		sourceText: `let (port : Int).withOffset (offset : Int) : Int := port + offset;

let publicPort := 8080.withOffset(1);
publicPort;`,
		evidence: {
			path: "docs/what/language/developers/csharp/extension-methods-and-calls.md",
			line: 1,
		},
	},
	{
		id: "csharp-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/csharp/namespaces-modules-packages.md",
			line: 1,
		},
	},
	{
		id: "csharp-module-import",
		language: "musi",
		sourceText: `let Defaults := import "./defaults";

let port := Defaults.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/csharp/namespaces-modules-packages.md",
			line: 1,
		},
	},
	{
		id: "csharp-testing-tooling",
		language: "musi",
		sourceText: `let Testing := import "@std/testing";

let defaultPort () : Int := 8080;

export let test () :=
  (
    Testing.describe("ports");
    Testing.it("default port is http alt", Testing.toBe(defaultPort(), 8080));
    Testing.endDescribe()
  );`,
		evidence: {
			path: "docs/what/language/developers/csharp/testing-tooling.md",
			line: 1,
		},
	},
	{
		id: "csharp-unsafe-interop-ffi",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

foreign "c" let puts (message : Ffi.CString) : Ffi.CInt;

export let announce (message : Ffi.CString) : Ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/csharp/unsafe-interop-ffi.md",
			line: 1,
		},
	},
	{
		id: "csharp-ffi-pointer",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

let pointer := Ffi.ptr.null[Int]();
let samePointer := unsafe { Ffi.ptr.offset[Int](pointer, 0); };
Ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/csharp/unsafe-interop-ffi.md",
			line: 1,
		},
	},
	{
		id: "js-ts-values-functions",
		language: "musi",
		sourceText: `let total (base : Int, fee : Int) : Int := base + fee;

let answer := total(1200, 45);
answer;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/values-functions.md",
			line: 1,
		},
	},
	{
		id: "js-ts-named-calls",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/values-functions.md",
			line: 1,
		},
	},
	{
		id: "js-ts-mutable-state",
		language: "musi",
		sourceText: `let visits := mut 0;
visits := visits + 1;
visits;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/state.md",
			line: 1,
		},
	},
	{
		id: "js-ts-fresh-value",
		language: "musi",
		sourceText: `let base := 1200;
let total := base + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/state.md",
			line: 1,
		},
	},
	{
		id: "js-ts-object-record",
		language: "musi",
		sourceText: `let Endpoint := data {
  host : String;
  port : Int;
  secure : Bool;
};

let local := {
  host := "localhost",
  port := 8080,
  secure := 0 = 1
};

let secure := { ...local, secure := 0 = 0 };
secure.port;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/objects-records.md",
			line: 1,
		},
	},
	{
		id: "js-ts-arrays-pipelines",
		language: "musi",
		sourceText: `let Iter := import "@std/iter";

let ports := [3000, 8080, 9000];
let visible := ports
  |> Iter.map[Int, Int](\\(port : Int) : Int => port + 1);
visible;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/arrays-pipelines.md",
			line: 1,
		},
	},
	{
		id: "js-ts-null-option",
		language: "musi",
		sourceText: `let Option := import "@std/option";

let findPort (name : String) : Option.Option[Int] := Option.some[Int](8080);

let port := findPort("local")
  |> Option.unwrapOr[Int](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/null-result.md",
			line: 1,
		},
	},
	{
		id: "js-ts-result-data",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsePort (text : String) : Result.Result[Int, String] := Result.ok[Int, String](8080);

let port := parsePort("8080")
  |> Result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/null-result.md",
			line: 1,
		},
	},
	{
		id: "js-ts-union-variant",
		language: "musi",
		sourceText: `let LoadState := data {
  | Loading
  | Loaded(value : Int)
  | Failed(message : String)
};

let state : LoadState := .Loaded(value := 8080);
match state (
| .Loaded\\(value) => value
| .Loading => 3000
| .Failed(message) => 3000
);`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/unions-variants.md",
			line: 1,
		},
	},
	{
		id: "js-ts-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/generics.md",
			line: 1,
		},
	},
	{
		id: "js-ts-generic-data",
		language: "musi",
		sourceText: `let Box1[T] := data {
  | Box1(value : T)
};

let boxed := .Box1(value := 8080);
match boxed (
| .Box1\\(value) => value
);`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/generics.md",
			line: 1,
		},
	},
	{
		id: "js-ts-promise-effect",
		language: "musi",
		sourceText: `let Io := import "@std/io";

let name := Io.promptTrimmed("name> ");
Io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/promises-effects.md",
			line: 1,
		},
	},
	{
		id: "js-ts-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/modules-packages.md",
			line: 1,
		},
	},
	{
		id: "js-ts-module-import",
		language: "musi",
		sourceText: `let Ports := import "./ports";

let port := Ports.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/modules-packages.md",
			line: 1,
		},
	},
	{
		id: "js-ts-class-instance",
		language: "musi",
		sourceText: `let Vehicle[T] := class {
  let wheels(self : T) : Int;
};

let Car := data {
  | Car
};

let carVehicle := instance Vehicle[Car] {
  let wheels(self : Car) : Int := 4;
};`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/classes-behavior.md",
			line: 1,
		},
	},
	{
		id: "js-ts-testing-tooling",
		language: "musi",
		sourceText: `let Testing := import "@std/testing";

let defaultPort () : Int := 8080;

export let test () :=
  (
    Testing.describe("ports");
    Testing.it("default port is http alt", Testing.toBe(defaultPort(), 8080));
    Testing.endDescribe()
  );`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/testing-tooling.md",
			line: 1,
		},
	},
	{
		id: "python-values-functions",
		language: "musi",
		sourceText: `let total (base : Int, fee : Int) : Int := base + fee;

let answer := total(1200, 45);
answer;`,
		evidence: {
			path: "docs/what/language/developers/python/values-functions.md",
			line: 1,
		},
	},
	{
		id: "python-named-calls",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/python/values-functions.md",
			line: 1,
		},
	},
	{
		id: "python-blocks-control-flow",
		language: "musi",
		sourceText: `let rec totalSeats (remaining : Int, seats : Int) : Int :=
  match remaining (
  | 0 => seats
  | _ => totalSeats(remaining - 1, seats + 4)
  );

let seats := totalSeats(3, 0);
seats;`,
		evidence: {
			path: "docs/what/language/developers/python/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "python-block-expression",
		language: "musi",
		sourceText: `let invoiceTotal :=
  (
    let base := 1200;
    let fee := 45;
    base + fee
  );

invoiceTotal;`,
		evidence: {
			path: "docs/what/language/developers/python/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "python-names-mutation",
		language: "musi",
		sourceText: `let visits := mut 0;
visits := visits + 1;

let nextVisits := visits + 1;
nextVisits;`,
		evidence: {
			path: "docs/what/language/developers/python/names-mutation.md",
			line: 1,
		},
	},
	{
		id: "python-fresh-value",
		language: "musi",
		sourceText: `let base := 1200;
let total := base + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/python/names-mutation.md",
			line: 1,
		},
	},
	{
		id: "python-records-objects-dicts",
		language: "musi",
		sourceText: `let Endpoint := data {
  host : String;
  port : Int;
  secure : Bool;
};

let local := {
  host := "localhost",
  port := 8080,
  secure := 0 = 1
};

let public := { ...local, host := "api.example.com", secure := 0 = 0 };
public.host;`,
		evidence: {
			path: "docs/what/language/developers/python/records-objects-dicts.md",
			line: 1,
		},
	},
	{
		id: "python-collections-pipelines",
		language: "musi",
		sourceText: `let Iter := import "@std/iter";

let ports := [3000, 8080];
let visible := ports
  |> Iter.append[Int](9000)
  |> Iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/python/collections-pipelines.md",
			line: 1,
		},
	},
	{
		id: "python-none-option",
		language: "musi",
		sourceText: `let Option := import "@std/option";

let lookupPort (name : String) : Option.Option[Int] :=
  match name (
  | "admin" => Option.some[Int](9000)
  | _ => Option.none[Int]()
  );

let port := lookupPort("web")
  |> Option.unwrapOr[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/python/none-option-result.md",
			line: 1,
		},
	},
	{
		id: "python-result-value",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsePort (text : String) : Result.Result[Int, String] :=
  match text (
  | "8080" => Result.ok[Int, String](8080)
  | _ => Result.err[Int, String]("port must be numeric")
  );

let port := parsePort("abc")
  |> Result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/python/none-option-result.md",
			line: 1,
		},
	},
	{
		id: "python-exceptions-results",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsePort (text : String) : Result.Result[Int, String] :=
  match text (
  | "8080" => Result.ok[Int, String](8080)
  | _ => Result.err[Int, String]("invalid port")
  );

let port := parsePort("abc")
  |> Result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/python/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "python-effect-boundary",
		language: "musi",
		sourceText: `let Io := import "@std/io";

let name := Io.promptTrimmed("name> ");
Io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/python/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "python-data-variants-patterns",
		language: "musi",
		sourceText: `let TaskState := data {
  | Waiting
  | Running(id : Int)
  | Done(code : Int)
};

let state : TaskState := .Running(id := 42);
match state (
| .Running(id) => id
| .Waiting => 0
| .Done(code) => code
);`,
		evidence: {
			path: "docs/what/language/developers/python/data-variants-patterns.md",
			line: 1,
		},
	},
	{
		id: "python-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/python/types-generics-protocols.md",
			line: 1,
		},
	},
	{
		id: "python-protocol-class",
		language: "musi",
		sourceText: `let Sized[T] := class {
  let size(self : T) : Int;
};

let Crate := data {
  | Crate(items : Int)
};

let crateSized := instance Sized[Crate] {
  let size(self : Crate) : Int :=
    match self (
    | .Crate(items) => items
    );
};`,
		evidence: {
			path: "docs/what/language/developers/python/types-generics-protocols.md",
			line: 1,
		},
	},
	{
		id: "python-module-export",
		language: "musi",
		sourceText: `// ports.ms
export let defaultPort () : Int := 8080;`,
		evidence: {
			path: "docs/what/language/developers/python/modules-packages.md",
			line: 1,
		},
	},
	{
		id: "python-module-import",
		language: "musi",
		sourceText: `let Ports := import "./ports";

let port := Ports.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/python/modules-packages.md",
			line: 1,
		},
	},
	{
		id: "python-testing-tooling",
		language: "musi",
		sourceText: `let Testing := import "@std/testing";

let defaultPort () : Int := 8080;

export let test () :=
  (
    Testing.describe("ports");
    Testing.it("default port is http alt", Testing.toBe(defaultPort(), 8080));
    Testing.endDescribe()
  );`,
		evidence: {
			path: "docs/what/language/developers/python/testing-tooling.md",
			line: 1,
		},
	},
	{
		id: "python-native-unsafe-ffi",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

foreign "c" let puts (message : Ffi.CString) : Ffi.CInt;

export let announce (message : Ffi.CString) : Ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/python/native-unsafe-ffi.md",
			line: 1,
		},
	},
	{
		id: "python-ffi-pointer",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

let pointer := Ffi.ptr.null[Int]();
let samePointer := unsafe { Ffi.ptr.offset[Int](pointer, 0); };
Ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/python/native-unsafe-ffi.md",
			line: 1,
		},
	},
	{
		id: "rust-values-functions",
		language: "musi",
		sourceText: `let total (base : Int, fee : Int) : Int := base + fee;

let answer := total(1200, 45);
answer;`,
		evidence: {
			path: "docs/what/language/developers/rust/values-functions.md",
			line: 1,
		},
	},
	{
		id: "rust-named-arguments",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/rust/values-functions.md",
			line: 1,
		},
	},
	{
		id: "rust-mutation-counter",
		language: "musi",
		sourceText: `let visits := mut 0;
visits := visits + 1;
visits;`,
		evidence: {
			path: "docs/what/language/developers/rust/mutation.md",
			line: 1,
		},
	},
	{
		id: "rust-fresh-value",
		language: "musi",
		sourceText: `let base := 1200;
let total := base + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/rust/mutation.md",
			line: 1,
		},
	},
	{
		id: "rust-struct-record",
		language: "musi",
		sourceText: `let Endpoint := data {
  host : String;
  port : Int;
  secure : Bool;
};

let local := {
  host := "localhost",
  port := 8080,
  secure := 0 = 1
};

let secure := { ...local, secure := 0 = 0 };
secure.port;`,
		evidence: {
			path: "docs/what/language/developers/rust/records-structs.md",
			line: 1,
		},
	},
	{
		id: "rust-enum-data-match",
		language: "musi",
		sourceText: `let Port := data {
  | Configured(port : Int)
  | Default
};

let selected : Port := .Configured(port := 8080);
match selected (
| .Configured(port) => port
| .Default => 3000
);`,
		evidence: {
			path: "docs/what/language/developers/rust/enums-data.md",
			line: 1,
		},
	},
	{
		id: "rust-trait-class-law",
		language: "musi",
		sourceText: `let Vehicle[T] := class {
  let wheels(self : T) : Int;
  law atLeastFourWheels(vehicle : T) := vehicle.wheels() >= 4;
};

let Car := data {
  | Car
};

let carVehicle := instance Vehicle[Car] {
  let wheels(self : Car) : Int := 4;
};`,
		evidence: {
			path: "docs/what/language/developers/rust/traits-classes-laws.md",
			line: 1,
		},
	},
	{
		id: "rust-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/rust/generics.md",
			line: 1,
		},
	},
	{
		id: "rust-generic-data",
		language: "musi",
		sourceText: `let Box1[T] := data {
  | Box1(value : T)
};

let boxed := .Box1(value := 8080);
match boxed (
| .Box1\\(value) => value
);`,
		evidence: {
			path: "docs/what/language/developers/rust/generics.md",
			line: 1,
		},
	},
	{
		id: "rust-type-constructor-class",
		language: "musi",
		sourceText: `let Box1[T] := data {
  | Box1(value : T)
};

let Keeps[F : Type -> Type] := class {
  let keep(value : F[Int]) : F[Int];
};

let boxKeeps := instance Keeps[Box1] {
  let keep(value : Box1[Int]) : Box1[Int] := value;
};`,
		evidence: {
			path: "docs/what/language/developers/rust/generics.md",
			line: 1,
		},
	},
	{
		id: "rust-result-data",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsed : Result.Result[Int, String] := Result.ok[Int, String](8080);
parsed
  |> Result.unwrapOr[Int, String](3000);`,
		evidence: {
			path: "docs/what/language/developers/rust/results-effects.md",
			line: 1,
		},
	},
	{
		id: "rust-effect-request",
		language: "musi",
		sourceText: `let Io := import "@std/io";

let line := Io.readTrimmedLine();
line;`,
		evidence: {
			path: "docs/what/language/developers/rust/results-effects.md",
			line: 1,
		},
	},
	{
		id: "rust-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/rust/modules-packages.md",
			line: 1,
		},
	},
	{
		id: "rust-module-import",
		language: "musi",
		sourceText: `let Ports := import "./ports";

let port := Ports.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/rust/modules-packages.md",
			line: 1,
		},
	},
	{
		id: "rust-unsafe-ffi",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

foreign "c" let get_counter () : CPtr;

let counter := unsafe {
  Ffi.ptr.cast[Int](get_counter());
};`,
		evidence: {
			path: "docs/what/language/developers/rust/unsafe-ffi.md",
			line: 1,
		},
	},
	{
		id: "rust-testing-tooling",
		language: "musi",
		sourceText: `let Testing := import "@std/testing";

let defaultPort () : Int := 8080;

export let test () :=
  (
    Testing.describe("ports");
    Testing.it("default port is http alt", Testing.toBe(defaultPort(), 8080));
    Testing.endDescribe()
  );`,
		evidence: {
			path: "docs/what/language/developers/rust/testing-tooling.md",
			line: 1,
		},
	},
] as const;

export function snippetById(id: string) {
	return contentSnippets.find((snippet) => snippet.id === id);
}
