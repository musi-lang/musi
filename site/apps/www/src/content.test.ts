import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { exampleGroups } from "./content/example-registry";
import { contentSnippets } from "./content/snippet-registry";
import { renderedDocs, renderedSnippets } from "./generated-content";
import { nextScheme } from "./layout";

const BANNED_SNIPPET_PATTERNS = [/\bif\b/, /\bthen\b/, /\belse\b/, /==/];
const REQUIRED_DOC_HEADINGS = [
	"## What",
	"## Why",
	"## How",
	"## When",
	"## Analogy",
	"## Try it",
];

const docsDirectory = join(import.meta.dirname, "content", "docs");

describe("content generation", () => {
	it("cycles color scheme in the header order", () => {
		expect(nextScheme("light")).toBe("dark");
		expect(nextScheme("dark")).toBe("auto");
		expect(nextScheme("auto")).toBe("light");
	});

	it("emits dual-theme shiki markup", () => {
		expect(renderedSnippets.homeSampleHtml).toContain("github-light");
		expect(renderedSnippets.homeSampleHtml).toContain("github-dark");
	});

	it("keeps invalid syntax out of Musi snippets", () => {
		const musiSnippetSource = contentSnippets
			.filter((snippet) => snippet.language === "musi")
			.map((snippet) => snippet.sourceText)
			.join("\n");

		for (const banned of BANNED_SNIPPET_PATTERNS) {
			expect(musiSnippetSource).not.toMatch(banned);
		}
	});

	it("keeps docs markdown free of raw Musi fences", () => {
		const docsSource = readdirSync(docsDirectory)
			.filter((entry) => entry.endsWith(".md"))
			.map((entry) => readFileSync(join(docsDirectory, entry), "utf8"))
			.join("\n");

		expect(docsSource).not.toContain("```musi");
		expect(docsSource).not.toContain("@std/io");
		expect(docsSource).not.toContain("let Option[T] := data");
		expect(docsSource).not.toContain("let Result[T, E] := data");
		for (const heading of REQUIRED_DOC_HEADINGS) {
			expect(docsSource).toContain(heading);
		}
	});

	it("fully resolves snippet and example placeholders during generation", () => {
		const docsHtml = renderedDocs.map((doc) => doc.html).join("\n");
		expect(docsHtml).not.toContain("{{snippet:");
		expect(docsHtml).not.toContain("{{example:");
	});

	it("records evidence for every snippet", () => {
		for (const snippet of contentSnippets) {
			expect(snippet.evidence.path.length).toBeGreaterThan(0);
			expect(snippet.evidence.line).toBeGreaterThan(0);
		}
	});

	it("requires complete four-language compare groups", () => {
		for (const group of exampleGroups) {
			expect(group.defaultLanguage).toBe("musi");
			expect(Object.keys(group.variants)).toEqual([
				"java",
				"musi",
				"rust",
				"typescript",
			]);
		}
	});

	it("renders compare tabs into generated html", () => {
		const docsHtml = renderedDocs.map((doc) => doc.html).join("\n");
		expect(renderedSnippets.homeSampleHtml).toContain('data-code-tabs="1"');
		expect(docsHtml).toContain('role="tablist"');
		expect(docsHtml).toContain("TypeScript");
	});
});
