import type { ExampleGroup } from "./types";

export const advancedExampleGroups: readonly ExampleGroup[] = [
	{
		id: "quote-metaprogramming",
		title: "Code as data template (metaprogramming)",
		caption:
			"Build or transform code structure itself, not only runtime values.",
		note: "When a language lacks first-class quote/splice, the closest equivalent is usually macros, AST builders, or plain code generators.",
		sourceText: "let addTemplate := quote (#(x) + #(delta));",
		evidence: {
			path: "crates/music_syntax/src/parser/tests.rs",
			line: 76,
		},
	},
	{
		id: "effect-handle",
		title: "Define, perform, and handle effects",
		caption:
			"Capture side-effect requests in one place, then resolve them through handlers.",
		note: "At small scale this can look like callback wiring, but at larger scale handlers keep policy at boundaries and reduce plumbing across call chains.",
		sourceText: `let console := effect {
  let readln () : String;
};

handle perform console.readln() with console of (
| readln(k) => resume "ok"
| value => value
);`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 263,
		},
	},
	{
		id: "class-instance",
		title: "Typeclass-style constraints and instances",
		caption:
			"Define shared behavior once, then attach concrete implementations per type.",
		note: "Like one wall-socket standard with different appliance designs behind the plug. Declare one behavior shape, then implement it per type.",
		sourceText: `let Eq[T] := class {
  let (=) (a : T, b : T) : Bool;
};

let eqInt := instance Eq[Int] {
  let (=) (a : Int, b : Int) : Bool := .True;
};`,
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 502,
		},
	},
	{
		id: "generic-constraint",
		title: "Generic constraint with explicit capability",
		caption:
			"Constrain polymorphic code to capabilities that must exist at call sites.",
		note: "Like requiring a driving license before renting a car: callers must provide the needed capability. Musi writes that requirement with <code>where T : Eq</code> before the result type annotation.",
		sourceText: "let requireEq[T] (x : T) where T : Eq : T := x;",
		evidence: {
			path: "crates/music_sema/src/tests.rs",
			line: 901,
		},
	},
] as const;
