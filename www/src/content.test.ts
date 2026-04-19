import { existsSync, readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { createElement, Fragment } from "preact/compat";
import { renderToString } from "preact-render-to-string";
import { describe, expect, it } from "vitest";
import { bookPages, bookParts, bookSections } from "./content/book/manifest";
import { contentCollections, languageGuideEntries } from "./content/catalog";
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
const repoRoot = join(import.meta.dirname, "..", "..");
const snippetEmbedPattern = /\{\{snippet:([\w-]+)\}\}/g;
const cFencePattern = /```c\n/;
const cppFencePattern = /```cpp\n/;
const nativeCppFunctionReturnPattern =
	/^\s*(?:bool|int|void|std::[A-Za-z0-9_:<>]+|[A-Z][A-Za-z0-9_:<>]*)\s+[A-Za-z_]\w*\s*\([^;{}]*\)\s*(?:const\s*)?(?:\{|;)/m;
const cppAutoFunctionWithoutTrailingReturnPattern =
	/^\s*auto\s+[A-Za-z_]\w*\s*\([^;{}]*\)\s*(?:const\s*)?\{/m;
const mutableCppResultBindingPattern =
	/(?:^|\n)auto\s+(?:answer|is_null|port|selected|visible)\s*=/;
const goFencePattern = /```go\n/;
const javaFencePattern = /```java\n/;
const luaFencePattern = /```lua\n/;
const topLevelLetPattern = /^let\s/;

function snippetIdsInMarkdown(source: string) {
	return [...source.matchAll(snippetEmbedPattern)].map((match) => match[1]);
}

function cppFencesInMarkdown(source: string) {
	return [...source.matchAll(/```cpp\n([\s\S]*?)\n```/g)].map(
		(match) => match[1],
	);
}

function frontmatterForMarkdown(source: string) {
	const [, frontmatter = ""] = source.split("---", 2);
	return frontmatter;
}

function hasGuideMetadata(source: string) {
	const frontmatter = frontmatterForMarkdown(source);

	return (
		frontmatter.includes("\ndescription: ") &&
		frontmatter.includes("\nsummary: ")
	);
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
			(entry) => entry.path === "/learn/book/start/foundations/values-and-let",
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

	it("keeps JavaScript and TypeScript guide examples paired with JS/TS snippets", () => {
		for (const page of bookPages) {
			if (
				!page.sourcePath.startsWith(
					"docs/what/language/developers/javascript-typescript",
				)
			) {
				continue;
			}

			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			const snippetIds = snippetIdsInMarkdown(source);

			expect(snippetIds.length, page.sourcePath).toBeGreaterThan(0);
			expect(
				snippetIds.every((snippetId) => snippetId.startsWith("js-ts-")),
				page.sourcePath,
			).toBe(true);
		}

		const overviewSource = readFileSync(
			join(
				repoRoot,
				"docs/what/language/developers/javascript-typescript/overview.md",
			),
			"utf8",
		);

		expect(overviewSource).toContain("TypeScript 5.9");
		expect(
			contentSnippets.some(
				(snippet) =>
					snippet.id === "guide-javascript-developers" ||
					snippet.id === "guide-typescript-developers",
			),
		).toBe(false);
	});

	it("keeps C99 guide examples paired with C99 snippets", () => {
		for (const page of bookPages) {
			if (!page.sourcePath.startsWith("docs/what/language/developers/c99")) {
				continue;
			}

			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			const snippetIds = snippetIdsInMarkdown(source);

			expect(hasGuideMetadata(source), page.sourcePath).toBe(true);
			expect(source, page.sourcePath).toMatch(cFencePattern);
			expect(source, page.sourcePath).not.toContain("{{example:");
			expect(snippetIds.length, page.sourcePath).toBeGreaterThan(0);
			expect(
				snippetIds.every((snippetId) => snippetId.startsWith("c99-")),
				page.sourcePath,
			).toBe(true);
		}

		const overviewSource = readFileSync(
			join(repoRoot, "docs/what/language/developers/c99/overview.md"),
			"utf8",
		);

		expect(overviewSource).toContain("C99");
	});

	it("keeps C++17 guide examples paired with C++17 snippets", () => {
		for (const page of bookPages) {
			if (!page.sourcePath.startsWith("docs/what/language/developers/cpp17")) {
				continue;
			}

			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			const snippetIds = snippetIdsInMarkdown(source);

			expect(hasGuideMetadata(source), page.sourcePath).toBe(true);
			expect(source, page.sourcePath).toMatch(cppFencePattern);
			expect(source, page.sourcePath).not.toContain("{{example:");
			expect(snippetIds.length, page.sourcePath).toBeGreaterThan(0);
			expect(
				snippetIds.every((snippetId) => snippetId.startsWith("cpp17-")),
				page.sourcePath,
			).toBe(true);
			for (const cppFence of cppFencesInMarkdown(source)) {
				expect(cppFence, page.sourcePath).not.toMatch(
					nativeCppFunctionReturnPattern,
				);
				expect(cppFence, page.sourcePath).not.toMatch(
					cppAutoFunctionWithoutTrailingReturnPattern,
				);
				expect(cppFence, page.sourcePath).not.toMatch(
					mutableCppResultBindingPattern,
				);
			}
		}

		const overviewSource = readFileSync(
			join(repoRoot, "docs/what/language/developers/cpp17/overview.md"),
			"utf8",
		);

		expect(overviewSource).toContain("C++17");
	});

	it("keeps C# guide examples paired with C# snippets", () => {
		for (const page of bookPages) {
			if (!page.sourcePath.startsWith("docs/what/language/developers/csharp")) {
				continue;
			}

			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			const snippetIds = snippetIdsInMarkdown(source);

			expect(snippetIds.length, page.sourcePath).toBeGreaterThan(0);
			expect(
				snippetIds.every((snippetId) => snippetId.startsWith("csharp-")),
				page.sourcePath,
			).toBe(true);
		}

		const overviewSource = readFileSync(
			join(repoRoot, "docs/what/language/developers/csharp/overview.md"),
			"utf8",
		);

		expect(overviewSource).toContain(".NET 8.0");
		expect(overviewSource).toContain("C# 12.0");
	});

	it("keeps Go guide examples paired with Go snippets", () => {
		for (const page of bookPages) {
			if (!page.sourcePath.startsWith("docs/what/language/developers/go/")) {
				continue;
			}

			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			const snippetIds = snippetIdsInMarkdown(source);

			expect(snippetIds.length, page.sourcePath).toBeGreaterThan(0);
			expect(
				snippetIds.every((snippetId) => snippetId.startsWith("go-")),
				page.sourcePath,
			).toBe(true);
			expect(goFencePattern.test(source), page.sourcePath).toBe(true);
		}

		const overviewSource = readFileSync(
			join(repoRoot, "docs/what/language/developers/go/overview.md"),
			"utf8",
		);

		expect(overviewSource).toContain("Go 1.26.2");
	});

	it("keeps Java guide examples paired with Java snippets", () => {
		for (const page of bookPages) {
			if (!page.sourcePath.startsWith("docs/what/language/developers/java/")) {
				continue;
			}

			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			const snippetIds = snippetIdsInMarkdown(source);

			expect(snippetIds.length, page.sourcePath).toBeGreaterThan(0);
			expect(
				snippetIds.every((snippetId) => snippetId.startsWith("java-")),
				page.sourcePath,
			).toBe(true);
			expect(javaFencePattern.test(source), page.sourcePath).toBe(true);
		}

		const overviewSource = readFileSync(
			join(repoRoot, "docs/what/language/developers/java/overview.md"),
			"utf8",
		);

		expect(overviewSource).toContain("Java 17");
	});

	it("keeps Lua guide examples paired with Lua snippets", () => {
		for (const page of bookPages) {
			if (!page.sourcePath.startsWith("docs/what/language/developers/lua/")) {
				continue;
			}

			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			const snippetIds = snippetIdsInMarkdown(source);

			expect(snippetIds.length, page.sourcePath).toBeGreaterThan(0);
			expect(
				snippetIds.every((snippetId) => snippetId.startsWith("lua-")),
				page.sourcePath,
			).toBe(true);
			expect(luaFencePattern.test(source), page.sourcePath).toBe(true);
		}

		const overviewSource = readFileSync(
			join(repoRoot, "docs/what/language/developers/lua/overview.md"),
			"utf8",
		);

		expect(overviewSource).toContain("Lua 5.4.8");
	});

	it("keeps Python guide examples paired with Python snippets", () => {
		for (const page of bookPages) {
			if (!page.sourcePath.startsWith("docs/what/language/developers/python")) {
				continue;
			}

			const source = readFileSync(join(repoRoot, page.sourcePath), "utf8");
			const snippetIds = snippetIdsInMarkdown(source);

			expect(snippetIds.length, page.sourcePath).toBeGreaterThan(0);
			expect(
				snippetIds.every((snippetId) => snippetId.startsWith("python-")),
				page.sourcePath,
			).toBe(true);
		}

		const overviewSource = readFileSync(
			join(repoRoot, "docs/what/language/developers/python/overview.md"),
			"utf8",
		);

		expect(overviewSource).toContain("Python 3.14");
	});

	it("keeps developer guides from redefining stdlib result and option shapes", () => {
		for (const snippet of contentSnippets) {
			if (!snippet.evidence.path.startsWith("docs/what/language/developers/")) {
				continue;
			}

			expect(
				snippet.sourceText,
				`${snippet.id} redefines a stdlib-shaped type`,
			).not.toMatch(BANNED_DEVELOPER_GUIDE_STDLIB_REDEFINITION_PATTERN);
			expect(
				snippet.sourceText,
				`${snippet.id} defines fake stdin`,
			).not.toMatch(BANNED_DEVELOPER_GUIDE_FAKE_STDIN_PATTERN);
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

	it("fully resolves snippet and example placeholders during generation", () => {
		const docsHtml = renderedDocs.map((doc) => doc.html).join("\n");
		expect(docsHtml).not.toContain("{{snippet:");
		expect(docsHtml).not.toContain("{{example:");
		expect(docsHtml).not.toContain("{{try:");
	});

	it("renders C and C++ guide examples as native fences plus snippets", () => {
		const guideDocs = renderedDocs.filter(
			(doc) =>
				doc.kind === "chapter" &&
				(doc.sectionId === "developers-c99" ||
					doc.sectionId === "developers-cpp17"),
		);

		expect(guideDocs.length).toBeGreaterThan(0);
		for (const doc of guideDocs) {
			expect(doc.html, doc.id).toContain("<pre");
			expect(doc.html, doc.id).toContain('class="mx-code-frame"');
		}
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
