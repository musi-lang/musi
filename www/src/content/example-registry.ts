import type { SnippetEvidence } from "./snippet-registry";

export type ComparisonLanguage = "java" | "musi" | "rust" | "typescript";

export interface ExampleVariant {
	language: ComparisonLanguage;
	sourceText: string;
	evidence: SnippetEvidence;
}

export interface ExampleGroup {
	id: string;
	title: string;
	caption: string;
	note: string;
	defaultLanguage: "musi";
	variants: Record<ComparisonLanguage, ExampleVariant>;
}

export const exampleGroups: readonly ExampleGroup[] = [
	{
		id: "option-fallback",
		title: "Fallback value from an optional result",
		caption:
			"One value may be present or missing. Musi does this with the real stdlib <code>Option</code> family.",
		note: "Musi uses imported constructors and <code>case</code>, not method chaining or statement-style branching.",
		defaultLanguage: "musi",
		variants: {
			java: {
				language: "java",
				sourceText: `Optional<Integer> configured = Optional.of(8080);
int port = configured.orElse(3000);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 22,
				},
			},
			musi: {
				language: "musi",
				sourceText: `let configured := Option.some[Int](8080);
Option.unwrap_or[Int](configured, 3000);`,
				evidence: {
					path: "packages/std/option/index.ms",
					line: 6,
				},
			},
			rust: {
				language: "rust",
				sourceText: `let configured = Some(8080);
let port = configured.unwrap_or(3000);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 39,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `const configured: number | null = 8080;
const port = configured ?? 3000;`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 46,
				},
			},
		},
	},
	{
		id: "double-function",
		title: "Small function that doubles a number",
		caption:
			"Same small task across four languages. Musi keeps it as an expression-oriented <code>let</code> binding.",
		note: "Musi functions are ordinary bindings, so the syntax stays close to other definitions.",
		defaultLanguage: "musi",
		variants: {
			java: {
				language: "java",
				sourceText: `static int twice(int x) {
    return x + x;
}

int answer = twice(21);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 63,
				},
			},
			musi: {
				language: "musi",
				sourceText: `let twice (x : Int) : Int := x + x;

twice(21);`,
				evidence: {
					path: "docs/what/language/syntax.md",
					line: 5,
				},
			},
			rust: {
				language: "rust",
				sourceText: `fn twice(x: i32) -> i32 {
    x + x
}

let answer = twice(21);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 78,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `function twice(x: number): number {
  return x + x;
}

const answer = twice(21);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 85,
				},
			},
		},
	},
	{
		id: "import-stdlib",
		title: "Import a standard library module",
		caption:
			"Import the standard library, then reach the family you need. Musi keeps stdlib access explicit through <code>@std</code>.",
		note: "Musi package imports are values, so <code>@std</code> becomes a normal binding you can pass around.",
		defaultLanguage: "musi",
		variants: {
			java: {
				language: "java",
				sourceText: "import java.util.Optional;",
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 102,
				},
			},
			musi: {
				language: "musi",
				sourceText: `let Std := import "@std";
let Option := Std.Option;`,
				evidence: {
					path: "packages/std/index.ms",
					line: 2,
				},
			},
			rust: {
				language: "rust",
				sourceText: "use std::option::Option;",
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 115,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `import { readFileSync } from "node:fs";`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 121,
				},
			},
		},
	},
	{
		id: "testing-entry",
		title: "Single package test entry",
		caption:
			"A small test entry should read like ordinary code. Musi uses <code>export let test ()</code> inside <code>*.test.ms</code> files.",
		note: "Musi package tests are discovered by file name, then run through <code>musi test</code>.",
		defaultLanguage: "musi",
		variants: {
			java: {
				language: "java",
				sourceText: `@Test
void sums_values() {
    assertEquals(3, 1 + 2);
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 138,
				},
			},
			musi: {
				language: "musi",
				sourceText: `let Testing := import "@std/testing";

export let test () :=
  Testing.it("adds values", Testing.to_be(1 + 2, 3));`,
				evidence: {
					path: "packages/std/option/index.test.ms",
					line: 4,
				},
			},
			rust: {
				language: "rust",
				sourceText: `#[test]
fn sums_values() {
    assert_eq!(1 + 2, 3);
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 154,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `test("adds values", () => {
  expect(1 + 2).toBe(3);
});`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 161,
				},
			},
		},
	},
] as const;

export function exampleGroupById(id: string) {
	return exampleGroups.find((group) => group.id === id);
}
