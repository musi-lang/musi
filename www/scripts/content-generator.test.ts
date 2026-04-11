import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { isWatchedContentPath } from "./content-generator";

const root = import.meta.dirname;

describe("content watch paths", () => {
	it("tracks docs, registries, grammar, and generator sources", () => {
		expect(
			isWatchedContentPath(
				join(root, "..", "src", "content", "docs", "first-program.md"),
			),
		).toBe(true);
		expect(
			isWatchedContentPath(
				join(root, "..", "src", "content", "example-registry.ts"),
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
