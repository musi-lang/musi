import { join } from "node:path";
import { describe, expect, it } from "vitest";
import {
	isWatchedContentPath,
	renderHighlightedCodeForTest,
} from "./content-generator";

const root = import.meta.dirname;
const letTokenPattern = /<span style="color:[^"]+">let<\/span>/;
const numberTokenPattern = /<span style="color:[^"]+"> 8080<\/span>/;
const functionNamePattern = /<span style="color:[^"]+">\s*identity_fn<\/span>/;
const genericTypePattern = /<span style="color:[^"]+">T<\/span>/;
const plainValueBindingPattern = /> port : <\/span>/;
const splitCallLikeBindingPattern = />\s*port<\/span>/;

describe("content watch paths", () => {
	it("tracks docs, registries, grammar, and generator sources", () => {
		expect(
			isWatchedContentPath(
				join(root, "..", "src", "content", "docs", "first-program.md"),
			),
		).toBe(true);
		expect(
			isWatchedContentPath(
				join(root, "..", "src", "content", "examples", "groups-core.ts"),
			),
		).toBe(true);
		expect(
			isWatchedContentPath(
				join(root, "..", "src", "content", "snippet-registry.ts"),
			),
		).toBe(true);
		expect(isWatchedContentPath(join(root, "content-generator.ts"))).toBe(true);
		expect(
			isWatchedContentPath(
				join(
					root,
					"..",
					"..",
					"vscode-ext",
					"syntaxes",
					"musi.tmLanguage.json",
				),
			),
		).toBe(true);
		expect(isWatchedContentPath(join(root, "..", "src", "pages.tsx"))).toBe(
			false,
		);
	});
});

describe("musi highlighting", () => {
	it("renders musi snippets with token styles instead of plaintext fallback", () => {
		const html = renderHighlightedCodeForTest(
			`let port := 8080;
let label := "ready";
let next := port + 1;`,
			"musi",
		);

		expect(html).toContain('class="shiki');
		expect(html).toMatch(letTokenPattern);
		expect(html).toMatch(numberTokenPattern);
	});

	it("highlights function definitions and generic type identifiers", () => {
		const html = renderHighlightedCodeForTest(
			"let identity_fn[T] (input : T) : T := input;",
			"musi",
		);

		expect(html).toMatch(functionNamePattern);
		expect(html).toMatch(genericTypePattern);
	});

	it("keeps let data declarations and value bindings out of call-site highlighting", () => {
		const declarationHtml = renderHighlightedCodeForTest(
			`let Port := data {
  | Configured : Int
  | Default
};`,
			"musi",
		);
		const bindingHtml = renderHighlightedCodeForTest(
			"let port : Port := .Configured(8080);",
			"musi",
		);

		expect(declarationHtml).toMatch(letTokenPattern);
		expect(bindingHtml).toMatch(plainValueBindingPattern);
		expect(bindingHtml).toContain(".Configured");
		expect(bindingHtml).not.toMatch(splitCallLikeBindingPattern);
	});
});
