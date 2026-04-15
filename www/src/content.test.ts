import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { createElement, Fragment } from "react";
import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";
import { bookPages, bookParts } from "./content/book/manifest";
import { exampleGroups } from "./content/examples/groups";
import { contentSnippets } from "./content/snippet-registry";
import { renderedDocs, renderedSnippets } from "./generated-content";
import { nextScheme } from "./layout/site-layout";
import { siteCopy } from "./lib/site-copy";

const BANNED_SNIPPET_PATTERNS = [/\bif\b/, /\bthen\b/, /\belse\b/, /==/];
const BANNED_SITE_COPY = [
	"Friendly language. Real ideas. Small steps.",
	"Friendly first-language path, real language ideas, and small command-line steps that stay readable as projects grow.",
];
const repoRoot = join(import.meta.dirname, "..", "..");

describe("content generation", () => {
	it("cycles color scheme in the header order", () => {
		expect(nextScheme("light")).toBe("dark");
		expect(nextScheme("dark")).toBe("system");
		expect(nextScheme("system")).toBe("light");
	});

	it("emits dual-theme shiki markup", () => {
		expect(renderedSnippets.homeSampleHtml).toContain("github-light");
		expect(renderedSnippets.homeSampleHtml).toContain("github-dark");
		expect(renderedSnippets.homeSampleHtml).not.toContain("Source:");
	});

	it("keeps shared public copy free of old slogan text", () => {
		const descriptorText = renderToStaticMarkup(
			createElement(Fragment, null, siteCopy.home.description),
		);

		for (const phrase of BANNED_SITE_COPY) {
			expect(descriptorText).not.toContain(phrase);
		}
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
		const docsSource = [
			...bookPages.map((page) =>
				readFileSync(join(repoRoot, page.sourcePath), "utf8"),
			),
			...bookParts.map((part) =>
				readFileSync(join(repoRoot, part.sourcePath), "utf8"),
			),
		].join("\n");

		expect(docsSource).not.toContain("```musi");
		expect(docsSource).not.toContain("@std/io");
		expect(docsSource).not.toContain("let Option[T] := data");
		expect(docsSource).not.toContain("let Result[T, E] := data");
	});

	it("renders every manifest doc entry", () => {
		expect(renderedDocs).toHaveLength(bookPages.length + bookParts.length);
		expect(renderedDocs.filter((doc) => doc.kind === "chapter")).toHaveLength(
			bookPages.length,
		);
	});

	it("fully resolves snippet and example placeholders during generation", () => {
		const docsHtml = renderedDocs.map((doc) => doc.html).join("\n");
		expect(docsHtml).not.toContain("{{snippet:");
		expect(docsHtml).not.toContain("{{example:");
		expect(docsHtml).not.toContain("{{try:");
	});

	it("keeps chapters on repo-level docs paths", () => {
		for (const page of bookPages) {
			expect(page.sourcePath.startsWith("docs/what/language/")).toBe(true);
		}
	});

	it("records evidence for every snippet", () => {
		for (const snippet of contentSnippets) {
			expect(snippet.evidence.path.length).toBeGreaterThan(0);
			expect(snippet.evidence.line).toBeGreaterThan(0);
			const absolutePath = join(repoRoot, snippet.evidence.path);
			expect(existsSync(absolutePath), snippet.id).toBe(true);
			expect(
				snippet.evidence.line <=
					readFileSync(absolutePath, "utf8").split("\n").length,
				snippet.id,
			).toBe(true);
		}
		for (const group of exampleGroups) {
			expect(group.evidence.path.length).toBeGreaterThan(0);
			expect(group.evidence.line).toBeGreaterThan(0);
			const absolutePath = join(repoRoot, group.evidence.path);
			expect(existsSync(absolutePath), group.id).toBe(true);
			expect(
				group.evidence.line <=
					readFileSync(absolutePath, "utf8").split("\n").length,
				group.id,
			).toBe(true);
		}
	});

	it("keeps evidence internal instead of rendering repo source links", () => {
		expect(renderedDocs.map((doc) => doc.html).join("\n")).not.toContain(
			"Source:",
		);
	});

	it("requires complete compare groups", () => {
		for (const group of exampleGroups) {
			expect(typeof group.sourceText).toBe("string");
		}
	});
});
