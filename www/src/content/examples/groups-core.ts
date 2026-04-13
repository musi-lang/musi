import type { ExampleGroup } from "./types";

export const coreExampleGroups: readonly ExampleGroup[] = [
	{
		id: "home-intro",
		title: "Musi basic example",
		caption:
			"A real-world example: A function checking a JSON response and parsing coordinates.",
		note: "Musi is expression-oriented. Types, bindings, and branches flow sequentially.",
		sourceText: `let json := import "@std/json";

let Point := data {
  x : Float;
  y : Float;
};

let parseLocation (payload : String) : Result[Point, String] :=
  case json.parse(payload) of (
  | .Ok(doc) =>
      let x := doc.field("x").asFloat().unwrapOr(0.0);
      let y := doc.field("y").asFloat().unwrapOr(0.0);
      .Ok({ x := x, y := y })
  | .Err(e) => .Err(e)
  );`,
		evidence: {
			path: "www/src/content/examples/groups-core.ts",
			line: 6,
		},
	},
	{
		id: "option-fallback",
		title: "Fallback value from an optional result",
		caption:
			"One value may be present or missing. Musi does this with the real stdlib <code>Option</code> family.",
		note: "Think of a spare house key: if the usual key exists, use it; otherwise use the backup. Musi expresses that choice with explicit constructors and <code>case</code>.",
		sourceText: `let configured := Option.some[Int](8080);
let port := Option.unwrapOr[Int](configured, 3000);`,
		evidence: {
			path: "packages/std/option/index.ms",
			line: 6,
		},
	},
	{
		id: "double-function",
		title: "Small function that doubles a number",
		caption:
			"Same small task across multiple languages. Musi keeps it as an expression-oriented <code>let</code> binding.",
		note: "Like doubling a recipe: same operation, different kitchens. Musi keeps the function as a normal binding so it reads like other definitions.",
		sourceText: `let twice (x : Int) : Int := x + x;

let answer := twice(21);`,
		evidence: {
			path: "docs/what/language/syntax.md",
			line: 5,
		},
	},
	{
		id: "import-stdlib",
		title: "Import a standard library module",
		caption:
			"Import the standard library, then reach the family you need. Musi keeps stdlib access explicit through <code>@std</code>.",
		note: "Like checking out a toolbox before work: import once, then use the tools by name. In Musi, imports are values you can pass around.",
		sourceText: `let Option := import "@std/option";

let value := Option.some(1);`,
		evidence: {
			path: "packages/std/option/index.ms",
			line: 2,
		},
	},
	{
		id: "testing-entry",
		title: "Single package test entry",
		caption:
			"A small test entry should read like ordinary code. Musi uses <code>export let test ()</code> inside <code>*.test.ms</code> files.",
		note: "Think smoke detector checks: small, repeatable, and run regularly. Musi discovers these by file name and runs them with <code>musi test</code>.",
		sourceText: `let Testing := import "@std/testing";
export let test () := Testing.it("adds values", Testing.toBe(1 + 2, 3));`,
		evidence: {
			path: "packages/std/option/index.test.ms",
			line: 4,
		},
	},
	{
		id: "data-named-record",
		title: "Named record data definition",
		caption:
			"Use named fields directly in a <code>data</code> definition, then construct values from that shape.",
		note: "Like filling out a passport form: named boxes with known defaults. This form keeps field intent explicit in the type itself.",
		sourceText: `let User := data {
  name : String;
  age : Int := 0;
};

let user : User := { name := "Ada" };`,
		evidence: {
			path: "grammar/Musi.g4",
			line: 157,
		},
	},
	{
		id: "record-array-spread",
		title: "Record and array spread updates",
		caption:
			"Build structured values, spread them, and update selected fields in one expression flow.",
		note: "Like copying a form and editing only one line instead of rewriting everything. Spread/update keeps the unchanged parts intact. Musi also has a nested record-update form inspired by F# and OCaml; the note below covers that separately.",
		sourceText: `let xs := [1, 2];
let ys := [0, ...xs, 3];

let p := { x := 1, y := 2 };
let q := { ...p, y := 9 };`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 1192,
		},
	},
] as const;
