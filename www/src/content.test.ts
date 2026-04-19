import { existsSync, readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { createElement, Fragment } from "preact/compat";
import { renderToString } from "preact-render-to-string";
import { describe, expect, it } from "vitest";
import { bookPages, bookParts, bookSections } from "./content/book/manifest";
import { contentCollections, languageGuideEntries } from "./content/catalog";
import {
	developerComparisonSpecs,
	developerComparisons,
	developerProfiles,
} from "./content/comparisons/developers";
import { exampleGroups } from "./content/examples/groups";
import { contentSnippets } from "./content/snippet-registry";
import { docSearchEntries } from "./docs";
import { renderedDocs, renderedSnippets } from "./generated-content";
import { siteCopy } from "./lib/site-copy";

const BANNED_SNIPPET_PATTERNS = [/\bif\b/, /\bthen\b/, /\belse\b/, /==/];
const BARE_MUSI_LAMBDA_PATTERN = /(?<![\\A-Za-z0-9_.])\([^()\n]*\)\s*=>/;
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
const BANNED_DEVELOPER_GUIDE_STDLIB_REDEFINITION_PATTERN =
	/\blet\s+(?:Maybe|Option|Result|[A-Za-z]+Result)(?:\[[^\]]+\])?\s*:=\s*data\b/;
const BANNED_DEVELOPER_GUIDE_FAKE_STDIN_PATTERN =
	/Console\.readLine|console\.readLine/;
const rawMusiIndexPattern =
	/(?<![A-Za-z0-9_.])(?:[a-z][A-Za-z0-9_]*)(?:\.[a-z][A-Za-z0-9_]*)*\[[^\]\n]+\](?!\s*\()/;
const BANNED_DEVELOPER_GUIDE_BOILERPLATE = [
	"This page is a translation note",
	"Start with the nouns in the example before reading the syntax",
	"Bring the useful instinct",
	"word-for-word port",
	"Musi examples are meant to make the domain shape visible first",
	"old habit under pressure",
	"Values use `let`, stored shape uses records or data",
];
const BANNED_MUSI_COMPARISON_PATTERNS = [
	/foreign\s+"[^"]+"\s*\{/,
	/\bfn\s+[A-Za-z_][A-Za-z0-9_]*\s*\(/,
	/unsafe\s+[A-Za-z_][A-Za-z0-9_]*(?:\.|)\(/,
	rawMusiIndexPattern,
];
const DEVELOPER_GUIDE_LANGUAGE_ANCHORS = [
	{
		path: "/c99/",
		pattern:
			/\b(?:C99|C reader|header|pointer|sentinel|errno|translation unit)\b/i,
	},
	{
		path: "/cpp17/",
		pattern: /\b(?:C\+\+17|C\+\+ reader|RAII|template|overload|optional)\b/i,
	},
	{
		path: "/csharp/",
		pattern: /\b(?:C#|CLR|LINQ|nullable|async|extension method)\b/i,
	},
	{
		path: "/go/",
		pattern: /\b(?:Go reader|goroutine|channel|nil|second return|package)\b/i,
	},
	{
		path: "/java/",
		pattern:
			/\b(?:Java reader|Java class|interface|exception|record|stream)\b/i,
	},
	{
		path: "/javascript-typescript/",
		pattern:
			/\b(?:TypeScript|JavaScript|undefined|promise|structural|prototype)\b/i,
	},
	{
		path: "/lua/",
		pattern: /\b(?:Lua reader|table|metatable|coroutine|nil|embedding)\b/i,
	},
	{
		path: "/python/",
		pattern: /\b(?:Python reader|None|duck|dict|protocol|exception)\b/i,
	},
	{
		path: "/rust",
		pattern: /\b(?:Rust reader|ownership|trait|impl|Result|unsafe)\b/i,
	},
] as const;
const repoRoot = join(import.meta.dirname, "..", "..");
const snippetEmbedPattern = /\{\{snippet:([\w-]+)\}\}/g;
const comparisonEmbedPattern = /\{\{compare:([\w-]+)\}\}/g;
const markdownLinkPattern = /\[[^\]\n]+\]\(([^)\s]+)(?:\s+"[^"]*")?\)/g;
const checkedInternalLinkPattern =
	/^\/(?:learn|docs)\/(?:book|language)\b|^\/learn\/guides\b/;
const topLevelLetPattern = /^let\s/;
const codeExampleEscapedNewlinePattern =
	/(?:sourceText|musiSourceText|code)\s*:\s*(["'])(?:(?:\\.)|(?!\1)[\s\S])*?\\n/;
const classMindsetPattern = /class.*(?:behavior|contract)|behavior.*class/i;
const markdownParagraphBreakPattern = /\n{2,}/;

function snippetIdsInMarkdown(source: string) {
	return [...source.matchAll(snippetEmbedPattern)].map((match) => match[1]);
}

function comparisonIdsInMarkdown(source: string) {
	return [...source.matchAll(comparisonEmbedPattern)].map((match) => match[1]);
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

function typescriptFilesInDirectory(root: string) {
	const files: string[] = [];
	for (const entry of readdirSync(root, { withFileTypes: true })) {
		const path = join(root, entry.name);
		if (entry.isDirectory()) {
			files.push(...typescriptFilesInDirectory(path));
			continue;
		}
		if (entry.name.endsWith(".ts") || entry.name.endsWith(".tsx")) {
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

function markdownBody(source: string) {
	if (!source.startsWith("---\n")) {
		return source;
	}
	const end = source.indexOf("\n---\n", 4);
	return end === -1 ? source : source.slice(end + 5);
}

function proseParagraphs(source: string) {
	return markdownBody(source)
		.split(markdownParagraphBreakPattern)
		.map((paragraph) => paragraph.trim())
		.filter(
			(paragraph) =>
				paragraph.length >= 90 &&
				!paragraph.startsWith("- [") &&
				!paragraph.startsWith("{{") &&
				!paragraph.startsWith("## "),
		);
}

function topLevelLetCount(source: string) {
	return source.split("\n").filter((line) => topLevelLetPattern.test(line))
		.length;
}

function normalizedRoutePath(path: string) {
	if (path === "/") {
		return path;
	}
	return path.endsWith("/") ? path.slice(0, -1) : path;
}

function renderedDocRoutePaths() {
	const paths = new Set<string>();
	for (const doc of renderedDocs) {
		paths.add(normalizedRoutePath(doc.path));
		paths.add(normalizedRoutePath(doc.canonicalPath));
		for (const alias of doc.aliases) {
			paths.add(normalizedRoutePath(alias));
		}
	}
	return paths;
}

function authoredSectionSourcePath(
	section: (typeof bookSections)[number],
): string | null {
	if (!("sourcePath" in section) || typeof section.sourcePath !== "string") {
		return null;
	}
	if (section.sourcePath.startsWith("generated://")) {
		return null;
	}
	return section.sourcePath;
}

describe("content generation", () => {
	it("emits light-only shiki markup", () => {
		expect(renderedSnippets.homeSampleHtml).toContain(
			"github-light-high-contrast",
		);
		expect(renderedSnippets.homeSampleHtml).not.toContain("Source:");
		for (const html of Object.values(renderedSnippets)) {
			expect(html).toContain("<pre");
			expect(html).not.toContain("mx-code-frame");
			expect(html).not.toContain("github-dark");
			expect(html).not.toContain("--shiki-dark");
		}
	});

	it("builds searchable docs entries from generated docs", () => {
		const valuesAndLet = docSearchEntries.find(
			(entry) => entry.path === "/learn/book/start/values-and-let",
		);

		expect(valuesAndLet?.title).toBe("Values and Let");
		expect(valuesAndLet?.searchText).toContain("let");
		expect(valuesAndLet?.summary.length).toBeGreaterThan(0);
		expect(valuesAndLet?.partTitle).toBe("Start");
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
		const musiSnippets = contentSnippets.filter(
			(snippet) => snippet.language === "musi",
		);
		const musiSnippetSource = musiSnippets
			.map((snippet) => snippet.sourceText)
			.join("\n");

		for (const banned of BANNED_SNIPPET_PATTERNS) {
			expect(musiSnippetSource).not.toMatch(banned);
		}
		for (const snippet of musiSnippets) {
			expect(snippet.sourceText, snippet.id).not.toMatch(
				BARE_MUSI_LAMBDA_PATTERN,
			);
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
		expect(docsSource).not.toContain("Console.readLine");
		expect(docsSource).not.toContain("console.readLine");
		expect(docsSource).not.toContain("let Option[T] := data");
		expect(docsSource).not.toContain("let Result[T, E] := data");
		expect(docsSource).not.toContain("RustBaseline");
		expect(docsSource).not.toContain('.Baseline(version := "1.87.0")');
	});

	it("keeps developer guides as language-specific notes with tabbed comparisons", () => {
		const developerPages = bookPages.filter((page) =>
			page.sourcePath.startsWith("docs/what/language/developers"),
		);
		const comparisonsById = new Map(
			developerComparisons.map((comparison) => [comparison.id, comparison]),
		);

		expect(developerPages.length).toBeGreaterThan(0);
		for (const page of developerPages) {
			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			const comparisonIds = comparisonIdsInMarkdown(source);

			expect(source, page.sourcePath).not.toContain("{{snippet:");
			expect(source, page.sourcePath).not.toContain("```musi");
			expect(source, page.sourcePath).not.toContain("side-by-side");
			if (
				page.sourcePath.endsWith("/overview.md") ||
				page.id === "musi-for-rust-developers"
			) {
				expect(comparisonIds, page.sourcePath).toHaveLength(0);
				continue;
			}

			expect(comparisonIds, page.sourcePath).toHaveLength(1);
			const comparison = comparisonsById.get(comparisonIds[0] ?? "");
			expect(comparison, page.sourcePath).toBeDefined();
			expect(comparison?.musiLabel, page.sourcePath).toBe("Musi");
			expect(comparison?.musiLanguage, page.sourcePath).toBe("musi");
			expect(
				comparison?.sourceText.trim().length,
				page.sourcePath,
			).toBeGreaterThan(0);
			expect(
				comparison?.musiSourceText.trim().length,
				page.sourcePath,
			).toBeGreaterThan(0);
		}

		const overviewExpectations = [
			["docs/what/language/developers/c99/overview.md", "C99"],
			["docs/what/language/developers/cpp17/overview.md", "C++17"],
			[
				"docs/what/language/developers/csharp/overview.md",
				".NET 8.0 / C# 12.0",
			],
			["docs/what/language/developers/go/overview.md", "Go 1.26.2"],
			["docs/what/language/developers/java/overview.md", "Java 17"],
			[
				"docs/what/language/developers/javascript-typescript/overview.md",
				"TypeScript 5.9",
			],
			["docs/what/language/developers/lua/overview.md", "Lua 5.4.8"],
			["docs/what/language/developers/python/overview.md", "Python 3.14"],
		] as const;

		for (const [sourcePath, expectedText] of overviewExpectations) {
			const source = readFileSync(join(repoRoot, sourcePath), "utf8");
			expect(source, sourcePath).toContain(expectedText);
		}
	});

	it("keeps developer guides from redefining stdlib result and option shapes", () => {
		for (const page of bookPages) {
			if (!page.sourcePath.startsWith("docs/what/language/developers")) {
				continue;
			}

			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			expect(
				source,
				`${page.sourcePath} redefines a stdlib-shaped type`,
			).not.toMatch(BANNED_DEVELOPER_GUIDE_STDLIB_REDEFINITION_PATTERN);
			expect(source, `${page.sourcePath} defines fake stdin`).not.toMatch(
				BANNED_DEVELOPER_GUIDE_FAKE_STDIN_PATTERN,
			);
		}
	});

	it("keeps developer guides free of copied boilerplate", () => {
		const paragraphCounts = new Map<
			string,
			{ count: number; paths: string[] }
		>();
		for (const page of bookPages) {
			if (!page.sourcePath.startsWith("docs/what/language/developers")) {
				continue;
			}

			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			for (const phrase of BANNED_DEVELOPER_GUIDE_BOILERPLATE) {
				expect(source, `${page.sourcePath} contains ${phrase}`).not.toContain(
					phrase,
				);
			}
			for (const paragraph of proseParagraphs(source)) {
				const current = paragraphCounts.get(paragraph) ?? {
					count: 0,
					paths: [],
				};
				current.count += 1;
				current.paths.push(page.sourcePath);
				paragraphCounts.set(paragraph, current);
			}
		}

		for (const [paragraph, hit] of paragraphCounts) {
			expect(
				hit.count,
				`${hit.paths.slice(0, 4).join(", ")} repeat paragraph ${paragraph}`,
			).toBeLessThanOrEqual(3);
		}
	});

	it("keeps developer guides anchored in source-language mindset", () => {
		for (const page of bookPages) {
			if (!page.sourcePath.startsWith("docs/what/language/developers")) {
				continue;
			}
			const anchor = DEVELOPER_GUIDE_LANGUAGE_ANCHORS.find((candidate) =>
				page.sourcePath.includes(candidate.path),
			);
			if (!anchor) {
				continue;
			}
			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			expect(source, page.sourcePath).toMatch(anchor.pattern);
		}
	});

	it("keeps Musi comparison fixtures grammar-backed", () => {
		for (const comparison of developerComparisons) {
			for (const pattern of BANNED_MUSI_COMPARISON_PATTERNS) {
				expect(comparison.musiSourceText, comparison.id).not.toMatch(pattern);
			}
		}
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

	describe("content registry architecture", () => {
		it("keeps public registry modules as small facades", () => {
			const manifestSource = readFileSync(
				join(repoRoot, "www", "src", "content", "book", "manifest.ts"),
				"utf8",
			);
			const snippetRegistrySource = readFileSync(
				join(repoRoot, "www", "src", "content", "snippet-registry.ts"),
				"utf8",
			);

			expect(manifestSource.split("\n").length).toBeLessThanOrEqual(120);
			expect(snippetRegistrySource.split("\n").length).toBeLessThanOrEqual(20);
			expect(snippetRegistrySource).toContain("./snippets");
			expect(manifestSource).toContain("./registry/pages");
		});

		it("stores developer registry data in per-language modules", () => {
			const developerFiles = [
				"c99",
				"cpp17",
				"csharp",
				"go",
				"java",
				"js-ts",
				"lua",
				"python",
				"rust",
			];

			for (const file of developerFiles) {
				expect(
					existsSync(
						join(
							repoRoot,
							"www",
							"src",
							"content",
							"book",
							"registry",
							"pages",
							"developers",
							`${file}.ts`,
						),
					),
					file,
				).toBe(true);
				expect(
					existsSync(
						join(
							repoRoot,
							"www",
							"src",
							"content",
							"snippets",
							"developers",
							`${file}.ts`,
						),
					),
					file,
				).toBe(true);
			}
		});
	});

	it("keeps generated content TypeScript small", () => {
		const generatedContentSource = readFileSync(
			join(repoRoot, "www", "src", "generated-content.ts"),
			"utf8",
		);

		expect(generatedContentSource.length).toBeLessThan(10_000);
		expect(
			existsSync(
				join(repoRoot, "www", "src", "generated", "rendered-docs.json"),
			),
		).toBe(true);
		expect(
			existsSync(
				join(repoRoot, "www", "src", "generated", "rendered-snippets.json"),
			),
		).toBe(true);
	});

	it("renders every manifest doc entry", () => {
		expect(renderedDocs).toHaveLength(
			bookPages.length + bookParts.length + bookSections.length,
		);
		expect(renderedDocs.filter((doc) => doc.kind === "chapter")).toHaveLength(
			bookPages.length,
		);
		expect(languageGuideEntries.length).toBeGreaterThanOrEqual(20);
		expect(
			contentCollections.some((collection) => collection.title === "Musi Book"),
		).toBe(true);
	});

	it("includes beginner gap chapters in the Musi Book", () => {
		const chapterExpectations = [
			{
				id: "reading-musi-code",
				path: "/docs/book/start/reading-musi-code",
				text: "Names before mechanics",
			},
			{
				id: "option-and-result",
				path: "/docs/book/data/option-and-result",
				text: "Option is not a quiet null",
			},
			{
				id: "errors-and-recovery",
				path: "/docs/book/effects-runtime/errors-and-recovery",
				text: "Pick the smallest honest shape",
			},
		] as const;

		for (const expected of chapterExpectations) {
			const page = bookPages.find((candidate) => candidate.id === expected.id);
			const doc = renderedDocs.find(
				(candidate) => candidate.id === expected.id,
			);
			expect(page?.path, expected.id).toBe(expected.path);
			expect(doc?.html, expected.id).toContain(expected.text);
			expect(doc?.html, expected.id).toContain("mx-code-frame");
			expect(doc?.html, expected.id).not.toContain("{{snippet:");
		}
	});

	it("keeps language guides out of the Musi Book hierarchy", () => {
		expect(
			renderedDocs.some((doc) => doc.path === "/learn/book/developers"),
		).toBe(false);
		expect(
			renderedDocs.some((doc) => doc.path === "/learn/book/developers/guides"),
		).toBe(false);
		expect(
			renderedDocs.some((doc) =>
				[doc.path, doc.canonicalPath, ...doc.aliases].some((path) =>
					path.includes("/book/developers"),
				),
			),
		).toBe(false);
		expect(renderedDocs.some((doc) => doc.path === "/learn/guides")).toBe(true);
		expect(
			renderedDocs.some((doc) => doc.path === "/learn/guides/rust/mutation"),
		).toBe(true);
	});

	it("fully resolves snippet and example placeholders during generation", () => {
		const docsHtml = renderedDocs.map((doc) => doc.html).join("\n");
		expect(docsHtml).not.toContain("{{snippet:");
		expect(docsHtml).not.toContain("{{example:");
		expect(docsHtml).not.toContain("{{compare:");
		expect(docsHtml).not.toContain("{{try:");
	});

	it("keeps internal docs links resolvable", () => {
		const routePaths = renderedDocRoutePaths();
		const docsSources = [
			...bookPages.map((page) => ({
				sourcePath: page.sourcePath,
				source: readFileSync(join(repoRoot, page.sourcePath), "utf8"),
			})),
			...bookParts.map((part) => ({
				sourcePath: part.sourcePath,
				source: readFileSync(join(repoRoot, part.sourcePath), "utf8"),
			})),
			...bookSections
				.map((section) => authoredSectionSourcePath(section))
				.filter((sourcePath): sourcePath is string => sourcePath !== null)
				.map((sourcePath) => ({
					sourcePath,
					source: readFileSync(join(repoRoot, sourcePath), "utf8"),
				})),
		];

		for (const { sourcePath, source } of docsSources) {
			for (const match of source.matchAll(markdownLinkPattern)) {
				const href = match[1] ?? "";
				if (!checkedInternalLinkPattern.test(href)) {
					continue;
				}
				const [path = ""] = href.split("#", 1);
				expect(routePaths.has(normalizedRoutePath(path)), sourcePath).toBe(
					true,
				);
			}
		}
	});

	it("renders developer guides as prose-led comparisons", () => {
		const guideDocs = renderedDocs.filter(
			(doc) =>
				doc.kind === "chapter" &&
				doc.sectionId?.startsWith("developers-") &&
				!doc.path.endsWith("/overview"),
		);

		expect(guideDocs.length).toBeGreaterThan(0);
		for (const doc of guideDocs) {
			expect(doc.html, doc.id).toContain("Reading");
			expect(doc.html, doc.id).toContain("False friend");
			expect(doc.html, doc.id).toContain("When this pays off");
			expect(doc.html, doc.id).toContain("Keep close");
			expect(doc.html, doc.id).toContain("mx-code-tabs");
			expect(doc.html, doc.id).toContain('data-code-tab-panel="source"');
			expect(doc.html, doc.id).toContain('data-code-tab-panel="musi"');
			expect(doc.html, doc.id).not.toContain("{{snippet:");
			expect(doc.html, doc.id).not.toContain("{{compare:");
		}
	});

	it("renders developer overviews as patient mindset guides", () => {
		const overviewDocs = renderedDocs.filter(
			(doc) =>
				doc.kind === "chapter" &&
				doc.sectionId?.startsWith("developers-") &&
				doc.path.endsWith("/overview"),
		);

		expect(overviewDocs.length).toBeGreaterThan(0);
		for (const doc of overviewDocs) {
			expect(doc.html, doc.id).toContain("translation journal");
			expect(doc.html, doc.id).toContain("First false friend");
			expect(doc.html, doc.id).toContain("Habits that still help");
			expect(doc.html, doc.id).toContain("When to switch to the Musi Book");
			expect(doc.html, doc.id).not.toContain("{{snippet:");
			expect(doc.html, doc.id).not.toContain("{{compare:");
		}
	});

	it("uses Machines CodeTabs instead of a website-owned duplicate", () => {
		expect(
			existsSync(join(repoRoot, "www", "src", "ui", "code-tabs.tsx")),
		).toBe(false);
		expect(
			readFileSync(
				join(repoRoot, "www", "src", "site-pages", "install", "page.tsx"),
				"utf8",
			),
		).toContain('from "@musi-lang/machines/preact"');
		expect(
			readFileSync(
				join(repoRoot, "packages", "machines", "src", "preact.tsx"),
				"utf8",
			),
		).toContain("export function CodeTabs");
		expect(
			readFileSync(
				join(repoRoot, "www", "scripts", "generate-content.ts"),
				"utf8",
			),
		).not.toContain("function codeTabFrame");
	});

	it("keeps authored multiline code examples as template literals", () => {
		for (const path of typescriptFilesInDirectory(
			join(repoRoot, "www", "src", "content"),
		)) {
			expect(readFileSync(path, "utf8"), path).not.toMatch(
				codeExampleEscapedNewlinePattern,
			);
		}
	});

	it("keeps developer comparisons generated from fixture files", () => {
		expect(developerComparisonSpecs.length).toBe(developerComparisons.length);
		for (const spec of developerComparisonSpecs) {
			expect(
				existsSync(
					join(
						repoRoot,
						"www",
						"src",
						"content",
						"comparisons",
						"examples",
						spec.sourcePath,
					),
				),
				spec.id,
			).toBe(true);
			expect(
				existsSync(
					join(
						repoRoot,
						"www",
						"src",
						"content",
						"comparisons",
						"examples",
						spec.musiPath,
					),
				),
				spec.id,
			).toBe(true);
		}
	});

	it("shares repeated Musi comparison fixtures instead of copying files", () => {
		const pathsBySource = new Map<string, Set<string>>();
		for (const spec of developerComparisonSpecs) {
			const source = readFileSync(
				join(
					repoRoot,
					"www",
					"src",
					"content",
					"comparisons",
					"examples",
					spec.musiPath,
				),
				"utf8",
			).trim();
			const paths = pathsBySource.get(source) ?? new Set<string>();
			paths.add(spec.musiPath);
			pathsBySource.set(source, paths);
		}

		for (const paths of pathsBySource.values()) {
			expect([...paths], [...paths].join(", ")).toHaveLength(1);
		}
	});

	it("uses current Option constructor names in docs content", () => {
		const searchableContent = [
			renderedDocs.map((doc) => doc.html).join("\n"),
			Object.values(renderedSnippets).join("\n"),
			developerComparisons
				.flatMap((comparison) => [
					comparison.sourceText,
					comparison.musiSourceText,
				])
				.join("\n"),
			contentSnippets.map((snippet) => snippet.sourceText).join("\n"),
		].join("\n");

		expect(searchableContent).not.toContain("option.some[");
		expect(searchableContent).not.toContain("option.none[");
		expect(searchableContent).not.toContain("makeSome");
		expect(searchableContent).not.toContain("makeNone");
	});

	it("uses dotted index syntax in Musi docs examples", () => {
		const searchableSources = [
			...markdownFilesInDirectory(
				join(repoRoot, "docs", "what", "language"),
			).map((path) => [path, readFileSync(path, "utf8")] as const),
			...developerComparisons.map(
				(comparison) =>
					[
						`developer comparison ${comparison.id}`,
						comparison.musiSourceText,
					] as const,
			),
			...contentSnippets
				.filter((snippet) => snippet.language === "musi")
				.map(
					(snippet) =>
						[`content snippet ${snippet.id}`, snippet.sourceText] as const,
				),
		];

		for (const [label, source] of searchableSources) {
			expect(source, label).not.toMatch(rawMusiIndexPattern);
		}
	});

	it("teaches class mindset per source language", () => {
		for (const [profileId, profile] of Object.entries(developerProfiles)) {
			expect(profile.mindset.length, profileId).toBeGreaterThan(0);
		}
		for (const profileId of [
			"csharp",
			"java",
			"javascript-typescript",
			"lua",
			"python",
		] as const) {
			expect(developerProfiles[profileId].mindset.join(" "), profileId).toMatch(
				classMindsetPattern,
			);
		}
	});

	it("keeps chapters on repo-level docs paths", () => {
		for (const page of bookPages) {
			expect(page.sourcePath.startsWith("docs/what/language/")).toBe(true);
		}
	});

	it("records evidence for generated code examples", () => {
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
		for (const comparison of developerComparisons) {
			expect(comparison.evidence.path.length).toBeGreaterThan(0);
			expect(comparison.evidence.line).toBeGreaterThan(0);
			const absolutePath = join(repoRoot, comparison.evidence.path);
			expect(existsSync(absolutePath), comparison.id).toBe(true);
			expect(
				comparison.evidence.line <=
					readFileSync(absolutePath, "utf8").split("\n").length,
				comparison.id,
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
