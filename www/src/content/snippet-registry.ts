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
case port of (
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
  case x of (
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
let identity_fn[T] (input : T) : T := input;`,
		evidence: {
			path: "docs/what/language/type-system.md",
			line: 3,
		},
	},
	{
		id: "types-apply",
		language: "musi",
		sourceText: "identity_fn[Int](port);",
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
		sourceText: `case port of (
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
		id: "perform-console",
		language: "musi",
		sourceText: "perform console.readln();",
		evidence: {
			path: "grammar/Musi.g4",
			line: 205,
		},
	},
	{
		id: "handle-console",
		language: "musi",
		sourceText: `handle perform console.readln() with console of (
| value => value
| readln(k) => resume "ok"
);`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 331,
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
		sourceText: `let eq_int := instance Eq[Int] {
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
		sourceText: `let add_one (x : Int) : Int := x + 1;
let add_two (x : Int) : Int := x + 2;`,
		evidence: {
			path: "docs/what/language/metaprogramming.md",
			line: 1,
		},
	},
	{
		id: "quote-with-meta",
		language: "musi",
		sourceText: `let add_template := quote (x + #(delta));
let add_one_syntax := quote (#(x) + 1);
let add_two_syntax := quote (#(x) + 2);`,
		evidence: {
			path: "docs/what/language/metaprogramming.md",
			line: 8,
		},
	},
	{
		id: "install-source",
		language: "bash",
		sourceText: `git clone https://github.com/musi-lang/musi.git
cd musi
cargo build --release
export PATH="/path/to/musi/target/release:$PATH"`,
		evidence: {
			path: "README.md",
			line: 1,
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
Option.unwrap_or[Int](configured, 3000);`,
		evidence: {
			path: "packages/std/option/index.ms",
			line: 6,
		},
	},
	{
		id: "stdlib-result-import",
		language: "musi",
		sourceText: `let parsed := Result.ok[Int, String](8080);
Result.unwrap_or[Int, String](parsed, 3000);`,
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
  Testing.it("adds values", Testing.to_be(1 + 2, 3));`,
		evidence: {
			path: "packages/std/option/index.test.ms",
			line: 4,
		},
	},
] as const;

export function snippetById(id: string) {
	return contentSnippets.find((snippet) => snippet.id === id);
}
