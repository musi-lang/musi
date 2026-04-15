import { existsSync, readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { createElement, Fragment } from "preact/compat";
import { renderToString } from "preact-render-to-string";
import { describe, expect, it } from "vitest";
import { bookPages, bookParts, bookSections } from "./content/book/manifest";
import { contentCollections, languageGuideEntries } from "./content/catalog";
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
const BANNED_WEBSITE_DOC_STAGING_PATTERNS = [
	/\bv1\b/i,
	/\bMVP\b/,
	/for now/i,
	/later if needed/i,
	/future CMS/i,
	/temporary/i,
	/until stable/i,
	/raw Markdown editing is acceptable/i,
	/richer fields.*later/i,
];
const repoRoot = join(import.meta.dirname, "..", "..");
const snippetEmbedPattern = /\{\{snippet:([\w-]+)\}\}/g;
const topLevelLetPattern = /^let\s/;

function snippetIdsInMarkdown(source: string) {
	return [...source.matchAll(snippetEmbedPattern)].map((match) => match[1]);
}

function snippetSourcesForMarkdown(source: string) {
	return snippetIdsInMarkdown(source).map((snippetId) => {
		const snippet = contentSnippets.find(
			(candidate) => candidate.id === snippetId,
		);
		if (!snippet) {
			throw new Error(`missing snippet ${snippetId}`);
		}
		return snippet.sourceText;
	});
}

function markdownFilesInDirectory(root: string) {
	const files: string[] = [];
	for (const entry of readdirSync(root, { withFileTypes: true })) {
		const path = join(root, entry.name);
		if (entry.isDirectory()) {
			files.push(...markdownFilesInDirectory(path));
			continue;
		}
		if (entry.name.endsWith(".md")) {
			files.push(path);
		}
	}
	return files;
}

function snippetByIdForTest(id: string) {
	const snippet = contentSnippets.find((candidate) => candidate.id === id);
	if (!snippet) {
		throw new Error(`missing snippet ${id}`);
	}
	return snippet;
}

function hasBlankLine(source: string) {
	return source.split("\n").some((line) => line.trim() === "");
}

function topLevelLetCount(source: string) {
	return source.split("\n").filter((line) => topLevelLetPattern.test(line))
		.length;
}

describe("content generation", () => {
	it("cycles color scheme in the header order", () => {
		expect(nextScheme("light")).toBe("dark");
		expect(nextScheme("dark")).toBe("system");
		expect(nextScheme("system")).toBe("light");
	});

	it("emits dual-theme shiki markup", () => {
		expect(renderedSnippets.homeSampleHtml).toContain(
			"github-light-high-contrast",
		);
		expect(renderedSnippets.homeSampleHtml).toContain("github-dark");
		expect(renderedSnippets.homeSampleHtml).not.toContain("Source:");
	});

	it("keeps shared public copy free of old slogan text", () => {
		const descriptorText = renderToString(
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
		expect(docsSource).not.toContain("In this chapter");
		expect(docsSource).not.toContain("Why it matters");
		expect(docsSource).not.toContain("@std/io");
		expect(docsSource).not.toContain("let Option[T] := data");
		expect(docsSource).not.toContain("let Result[T, E] := data");
		expect(docsSource).not.toContain("RustBaseline");
		expect(docsSource).not.toContain('.Baseline(version := "1.87.0")');
	});

	it("keeps Rust guide examples paired with Rust-specific Musi snippets", () => {
		for (const page of bookPages) {
			if (!page.sourcePath.startsWith("docs/what/language/developers/rust")) {
				continue;
			}

			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			const snippetIds = snippetIdsInMarkdown(source);

			expect(snippetIds.length, page.sourcePath).toBeGreaterThan(0);
			expect(
				snippetIds.every((snippetId) => snippetId.startsWith("rust-")),
				page.sourcePath,
			).toBe(true);
		}

		expect(
			contentSnippets.some((snippet) => snippet.id === "guide-rust-developers"),
		).toBe(false);
		expect(
			contentSnippets.some((snippet) =>
				snippet.sourceText.includes("RustBaseline"),
			),
		).toBe(false);
	});

	it("keeps generic examples spaced by usage step", () => {
		const genericsSource = snippetByIdForTest("chapter-generics").sourceText;

		expect(genericsSource).toContain("let port := identityFn[Int](8080);");
		expect(genericsSource).toContain(
			"let copiedPort := tools.identity[Int](port);",
		);
		expect(genericsSource).toContain("| Box1(value : T)");
		expect(genericsSource).toContain("let Keeps[F : Type -> Type] := class");
		expect(genericsSource).toContain("\n\nlet tools :=");
		expect(genericsSource).toContain("\n\nlet Box1[T] :=");
		expect(genericsSource).toContain("\n\nlet Keeps[F : Type -> Type] :=");
		expect(genericsSource).not.toContain(
			"let tools := { identity := identityFn };",
		);
	});

	it("keeps long multi-declaration Musi snippets visually separated", () => {
		for (const snippet of contentSnippets) {
			if (snippet.language !== "musi") {
				continue;
			}

			const nonblankLines = snippet.sourceText
				.split("\n")
				.filter((line) => line.trim() !== "");
			if (
				nonblankLines.length < 8 ||
				topLevelLetCount(snippet.sourceText) < 4
			) {
				continue;
			}

			expect(hasBlankLine(snippet.sourceText), snippet.id).toBe(true);
		}
	});

	it("keeps recursive prose paired with recursive snippets", () => {
		const valuesAndLetSource = readFileSync(
			join(repoRoot, "docs/what/language/start/values-and-let.md"),
			"utf8",
		);

		expect(valuesAndLetSource).toContain("{{snippet:recursive-case}}");
		expect(valuesAndLetSource).not.toContain("{{snippet:chapter-functions}}");

		for (const page of bookPages) {
			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			if (!source.includes("let rec")) {
				continue;
			}

			const snippetSources = snippetSourcesForMarkdown(source);
			expect(
				snippetSources.some((snippetSource) =>
					snippetSource.includes("let rec"),
				),
				page.sourcePath,
			).toBe(true);
		}
	});

	it("keeps website docs free of staging language", () => {
		const websiteDocs = [
			join(repoRoot, "www/README.md"),
			...markdownFilesInDirectory(join(repoRoot, "docs/what/website")),
		];

		for (const path of websiteDocs) {
			const source = readFileSync(path, "utf8");
			for (const pattern of BANNED_WEBSITE_DOC_STAGING_PATTERNS) {
				expect(source, `${path} matches ${pattern}`).not.toMatch(pattern);
			}
		}
	});

	it("keeps website operations in the website README", () => {
		const rootReadme = readFileSync(join(repoRoot, "README.md"), "utf8");
		const websiteReadme = readFileSync(join(repoRoot, "www/README.md"), "utf8");
		const packageManifest = readFileSync(
			join(repoRoot, "package.json"),
			"utf8",
		);

		expect(rootReadme).toContain("[`www/README.md`](www/README.md)");
		expect(rootReadme).not.toContain("## Cloudflare Pages");
		expect(rootReadme).not.toContain("Pages CMS");
		expect(rootReadme).not.toContain("Build output directory: `www/dist`");
		expect(existsSync(join(repoRoot, ".pages.yml"))).toBe(false);
		expect(existsSync(join(repoRoot, "cms"))).toBe(false);
		expect(
			existsSync(join(repoRoot, "docs/what/website/pages-cms-hosting.md")),
		).toBe(false);
		expect(packageManifest).toContain(
			'"docs:studio": "bun run --cwd www docs:studio"',
		);
		expect(packageManifest).not.toContain("cms:");

		for (const heading of [
			"## Local Contributor",
			"## Docs Author",
			"## Local Docs Studio",
			"## Publisher Maintainer",
			"### Cloudflare Pages",
		]) {
			expect(websiteReadme).toContain(heading);
		}

		expect(websiteReadme).toContain("bun run dev");
		expect(websiteReadme).toContain("bun run docs:studio");
		expect(websiteReadme).toContain("http://127.0.0.1:4322");
		expect(websiteReadme).not.toContain("Pages CMS");
		expect(websiteReadme).not.toContain("Netlify");
		expect(websiteReadme).not.toContain("GitHub App");
		expect(websiteReadme).not.toContain("Postgres");
		expect(websiteReadme).not.toContain("npm ");
		expect(websiteReadme).not.toContain("npx ");
	});

	it("renders every manifest doc entry", () => {
		expect(renderedDocs).toHaveLength(
			bookPages.length + bookParts.length + bookSections.length,
		);
		expect(renderedDocs.filter((doc) => doc.kind === "chapter")).toHaveLength(
			bookPages.length,
		);
		expect(languageGuideEntries.length).toBeGreaterThanOrEqual(11);
		expect(
			contentCollections.some((collection) => collection.title === "Musi Book"),
		).toBe(true);
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
