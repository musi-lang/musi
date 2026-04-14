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
			path: "docs/how/toolchain.md",
			line: 1,
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
			path: "grammar/Musi.g4",
			line: 137,
		},
	},
	{
		id: "case-port",
		language: "musi",
		sourceText: `let Port := data {
  | Configured : Int
  | Default
};

let port : Port := .Configured(8080);
match port (
| .Configured(port) => port
| .Default => 3000
);`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1160,
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
let Option := Std.Option;
let Result := Std.Result;`,
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
			path: "docs/what/language/type-system.md",
			line: 3,
		},
	},
	{
		id: "types-apply",
		language: "musi",
		sourceText: "identityFn[Int](port);",
		evidence: {
			path: "docs/what/language/type-system.md",
			line: 8,
		},
	},
	{
		id: "data-port",
		language: "musi",
		sourceText: `let Port := data {
  | Configured : Int
  | Default
};`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1160,
		},
	},
	{
		id: "data-port-value",
		language: "musi",
		sourceText: "let port : Port := .Configured(8080);",
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1165,
		},
	},
	{
		id: "data-port-case",
		language: "musi",
		sourceText: `match port (
| .Configured(value) => value
| .Default => 3000
);`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1167,
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
			path: "grammar/Musi.g4",
			line: 153,
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
			path: "grammar/Musi.g4",
			line: 380,
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
			path: "docs/what/language/type-system.md",
			line: 3,
		},
	},
	{
		id: "effect-console",
		language: "musi",
		sourceText: `let console := effect {
  let readln () : String;
};`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 317,
		},
	},
	{
		id: "request-console",
		language: "musi",
		sourceText: "request console.readln();",
		evidence: {
			path: "grammar/Musi.g4",
			line: 205,
		},
	},
	{
		id: "handle-console",
		language: "musi",
		sourceText: `handle request console.readln() using console {
  value => value;
  readln(k) => resume "ok";
};`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 331,
		},
	},
	{
		id: "using-signature",
		language: "musi",
		sourceText: `let readClosed (x : Int) : String using { Console } :=
  request State.readln();`,
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
			path: "docs/what/language/metaprogramming.md",
			line: 1,
		},
	},
	{
		id: "quote-with-meta",
		language: "musi",
		sourceText: `let addTemplate := quote (x + #(delta));
let addOneSyntax := quote (#(x) + 1);
let addTwoSyntax := quote (#(x) + 2);`,
		evidence: {
			path: "docs/what/language/metaprogramming.md",
			line: 8,
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
			path: "crates/music_session/src/session/foundation.rs",
			line: 1,
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
		sourceText: `let configured := Option.some[Int](8080);
Option.unwrapOr[Int](configured, 3000);`,
		evidence: {
			path: "packages/std/option/index.ms",
			line: 6,
		},
	},
	{
		id: "stdlib-result-import",
		language: "musi",
		sourceText: `let parsed := Result.ok[Int, String](8080);
Result.unwrapOr[Int, String](parsed, 3000);`,
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
			path: "grammar/Musi.g4",
			line: 137,
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
			path: "grammar/Musi.g4",
			line: 380,
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
			path: "grammar/Musi.g4",
			line: 380,
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
		sourceText: `let twice (x : Int) : Int := x + x;
let answer := twice(21);
answer;`,
		evidence: {
			path: "docs/what/language/syntax.md",
			line: 5,
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
  | Configured : Int
  | Default
};

let port : Port := .Configured(8080);
match port (
| .Configured(value) => value
| .Default => 3000
);`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1160,
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
			path: "docs/what/language/type-system.md",
			line: 3,
		},
	},
	{
		id: "chapter-type-inference",
		language: "musi",
		sourceText: `let port : Int := 8080;
let next := port + 1;
next;`,
		evidence: {
			path: "docs/what/language/type-system.md",
			line: 3,
		},
	},
	{
		id: "chapter-generics",
		language: "musi",
		sourceText: `let identityFn[T] (input : T) : T := input;
let port : Int := 8080;
identityFn[Int](port);`,
		evidence: {
			path: "docs/what/language/type-system.md",
			line: 3,
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
		id: "chapter-effects",
		language: "musi",
		sourceText: `let console := effect {
  let readln () : String;
};

request console.readln();`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 317,
		},
	},
	{
		id: "chapter-using",
		language: "musi",
		sourceText: `let readClosed (x : Int) : String using { Console } :=
  request State.readln();`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1444,
		},
	},
	{
		id: "chapter-handlers",
		language: "musi",
		sourceText: `handle request console.readln() using console {
  value => value;
  readln(k) => resume "ok";
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
			path: "crates/music_session/src/session/foundation.rs",
			line: 1,
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
let configured := Option.some[Int](8080);
Option.unwrapOr[Int](configured, 3000);`,
		evidence: {
			path: "packages/std/option/index.ms",
			line: 6,
		},
	},
	{
		id: "chapter-attributes",
		language: "musi",
		sourceText:
			'@link(name := "c") foreign "c" let puts (msg : CString) : Int;',
		evidence: {
			path: "crates/music_syntax/src/parser/tests.rs",
			line: 78,
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
		id: "chapter-quote-and-syntax",
		language: "musi",
		sourceText: `let addTemplate := quote (x + #(delta));
let addOneSyntax := quote (#(x) + 1);
addOneSyntax;`,
		evidence: {
			path: "docs/what/language/metaprogramming.md",
			line: 8,
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
] as const;

export function snippetById(id: string) {
	return contentSnippets.find((snippet) => snippet.id === id);
}
