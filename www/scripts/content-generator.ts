import { readFile, writeFile } from "node:fs/promises";
import { dirname, join, normalize, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { marked } from "marked";
import { createHighlighter } from "shiki";
import {
	type ComparisonLanguage,
	exampleGroupById,
	exampleGroups,
} from "../src/content/example-registry";
import { contentSnippets, snippetById } from "../src/content/snippet-registry";

interface MarkdownDocumentAttributes {
	title: string;
	description: string;
	group: string;
	section: string;
	order: number;
	slug: string;
	summary: string;
}

interface GeneratedHeading {
	depth: number;
	id: string;
	text: string;
}

interface GeneratedDoc extends MarkdownDocumentAttributes {
	descriptionHtml: string;
	headings: GeneratedHeading[];
	html: string;
	summaryHtml: string;
}

const quotedValuePattern = /^"(.*)"$/;
const numberValuePattern = /^\d+$/;
const rawMusiFencePattern = /```musi\b/;
const examplePattern = /\{\{example:([\w-]+)\}\}/g;
const snippetPattern = /\{\{snippet:([\w-]+)\}\}/g;
const bannedSyntaxPatterns = [/\bif\b/, /\bthen\b/, /\belse\b/, /==/];
const stdlibRedefinitionPattern = /let\s+(Option|Result)\[[^\]]+\]\s*:=/;
const bannedDocsPatterns = [
	/current tests prove/i,
	/long-term design/i,
	/semantic collection/i,
	/compiler architecture/i,
	/current design/i,
	/@std\/io/,
];
const requiredSectionHeadings = [
	"## What",
	"## Why",
	"## How",
	"## When",
	"## Analogy",
	"## Try it",
];
const comparisonOrder: readonly ComparisonLanguage[] = [
	"java",
	"musi",
	"rust",
	"typescript",
] as const;

const scriptsDirectory = dirname(fileURLToPath(import.meta.url));
const appRoot = join(scriptsDirectory, "..");
const docsDirectory = join(appRoot, "src", "content", "docs");
const exampleRegistryPath = join(
	appRoot,
	"src",
	"content",
	"example-registry.ts",
);
const snippetRegistryPath = join(
	appRoot,
	"src",
	"content",
	"snippet-registry.ts",
);
const generatorModulePath = join(scriptsDirectory, "content-generator.ts");
const generatorEntrypointPath = join(scriptsDirectory, "generate-content.ts");
const grammarPath = join(
	scriptsDirectory,
	"..",
	"..",
	"vscode-ext",
	"syntaxes",
	"musi.tmLanguage.json",
);

export const generatedContentPath = join(
	appRoot,
	"src",
	"generated-content.ts",
);

export const watchedContentPaths = [
	docsDirectory,
	exampleRegistryPath,
	snippetRegistryPath,
	generatorModulePath,
	generatorEntrypointPath,
	grammarPath,
] as const;

function pathWithTrailingSeparator(path: string) {
	return `${normalize(path)}${sep}`;
}

export function isWatchedContentPath(path: string) {
	const normalized = normalize(path);
	return (
		normalized.startsWith(pathWithTrailingSeparator(docsDirectory)) ||
		normalized === normalize(exampleRegistryPath) ||
		normalized === normalize(snippetRegistryPath) ||
		normalized === normalize(generatorModulePath) ||
		normalized === normalize(generatorEntrypointPath) ||
		normalized === normalize(grammarPath)
	);
}

const highlighter = await createHighlighter({
	themes: ["github-light", "github-dark"],
	langs: [
		"bash",
		"json",
		"java",
		"plaintext",
		"rust",
		"toml",
		"typescript",
		{
			...(JSON.parse(await readFile(grammarPath, "utf8")) as Record<
				string,
				unknown
			>),
			name: "musi",
		} as never,
	],
});

function parseFrontmatter(source: string) {
	if (!source.startsWith("---\n")) {
		return { attributes: {}, body: source };
	}

	const boundary = source.indexOf("\n---\n", 4);
	if (boundary === -1) {
		return { attributes: {}, body: source };
	}

	const rawAttributes = source.slice(4, boundary).split("\n");
	const body = source.slice(boundary + 5).trimStart();
	const attributes: Record<string, string | number> = {};

	for (const line of rawAttributes) {
		const separator = line.indexOf(":");
		if (separator === -1) {
			continue;
		}
		const key = line.slice(0, separator).trim();
		const rawValue = line.slice(separator + 1).trim();
		const value = rawValue.replace(quotedValuePattern, "$1");
		attributes[key] = numberValuePattern.test(value) ? Number(value) : value;
	}

	return { attributes, body };
}

function normalizeLanguage(language: string) {
	switch (language) {
		case "shell":
		case "sh":
			return "bash";
		case "text":
			return "plaintext";
		default:
			return language;
	}
}

function slugifyHeading(text: string) {
	return text
		.toLowerCase()
		.replace(/[`']/g, "")
		.replace(/[^a-z0-9]+/g, "-")
		.replace(/^-+|-+$/g, "");
}

function validateSnippetSyntax(
	id: string,
	sourceText: string,
	language: string,
) {
	if (language !== "musi") {
		return;
	}

	for (const pattern of bannedSyntaxPatterns) {
		if (pattern.test(sourceText)) {
			throw new Error(`snippet ${id} contains banned syntax: ${pattern}`);
		}
	}

	if (stdlibRedefinitionPattern.test(sourceText)) {
		throw new Error(`snippet ${id} redefines stdlib construct`);
	}
}

function assertNoRawMusiFences(source: string, path: string) {
	if (rawMusiFencePattern.test(source)) {
		throw new Error(`${path} contains raw Musi code fences`);
	}
}

function assertConsumerSafeDocs(source: string, path: string) {
	for (const pattern of bannedDocsPatterns) {
		if (pattern.test(source)) {
			throw new Error(`${path} contains banned docs content: ${pattern}`);
		}
	}
}

function assertRequiredSections(source: string, path: string) {
	for (const heading of requiredSectionHeadings) {
		if (!source.includes(heading)) {
			throw new Error(`${path} is missing required section ${heading}`);
		}
	}
}

function renderSnippet(id: string) {
	const snippet = snippetById(id);
	if (!snippet) {
		throw new Error(`missing snippet ${id}`);
	}

	validateSnippetSyntax(snippet.id, snippet.sourceText, snippet.language);
	return highlighter.codeToHtml(snippet.sourceText, {
		lang: normalizeLanguage(snippet.language),
		themes: {
			light: "github-light",
			dark: "github-dark",
		},
	});
}

function escapeHtmlAttribute(value: string) {
	return value
		.replaceAll("&", "&amp;")
		.replaceAll('"', "&quot;")
		.replaceAll("<", "&lt;")
		.replaceAll(">", "&gt;");
}

function renderInlineHtml(source: string) {
	return marked.parseInline(source, { gfm: true }) as string;
}

async function renderExample(id: string) {
	const group = exampleGroupById(id);
	if (!group) {
		throw new Error(`missing example group ${id}`);
	}

	for (const language of comparisonOrder) {
		const variant = group.variants[language];
		if (!variant) {
			throw new Error(`example group ${id} is missing ${language}`);
		}
		validateSnippetSyntax(
			`${id}:${language}`,
			variant.sourceText,
			variant.language,
		);
	}

	const tabButtons = comparisonOrder
		.map((language) => {
			const active = language === group.defaultLanguage;
			return `<button type="button" role="tab" class="code-tab" data-language="${language}" aria-selected="${active ? "true" : "false"}" tabindex="${active ? "0" : "-1"}">${language === "typescript" ? "TypeScript" : language === "java" ? "Java" : language === "rust" ? "Rust" : "Musi"}</button>`;
		})
		.join("");

	const panels = await Promise.all(
		comparisonOrder.map((language) => {
			const variant = group.variants[language];
			const html = highlighter.codeToHtml(variant.sourceText, {
				lang: normalizeLanguage(variant.language),
				themes: {
					light: "github-light",
					dark: "github-dark",
				},
			});
			const hidden = language === group.defaultLanguage ? "" : ' hidden=""';
			return `<section role="tabpanel" class="code-panel" data-language="${language}" data-active="${language === group.defaultLanguage ? "true" : "false"}"${hidden}>${html}</section>`;
		}),
	);

	return `<div class="code-tabs" data-code-tabs="1" data-example-id="${escapeHtmlAttribute(id)}" data-default="${group.defaultLanguage}" data-active-language="${group.defaultLanguage}">
<div class="code-tabs-meta">
<p class="code-tabs-caption">${renderInlineHtml(group.caption)}</p>
<p class="code-tabs-note">${renderInlineHtml(group.note)}</p>
</div>
<div class="code-tablist" role="tablist" aria-label="${escapeHtmlAttribute(group.title)}">${tabButtons}</div>
${panels.join("\n")}
</div>`;
}

async function replaceContentPlaceholders(source: string) {
	const snippetMatches = Array.from(source.matchAll(snippetPattern));
	const exampleMatches = Array.from(source.matchAll(examplePattern));
	const matches = [...snippetMatches, ...exampleMatches].sort(
		(left, right) => (left.index ?? 0) - (right.index ?? 0),
	);
	if (matches.length === 0) {
		return source;
	}

	let cursor = 0;
	let result = "";

	for (const match of matches) {
		const start = match.index ?? cursor;
		const [matched, id] = match;
		result += source.slice(cursor, start);
		result += matched.startsWith("{{example:")
			? await renderExample(id)
			: await renderSnippet(id);
		cursor = start + matched.length;
	}

	result += source.slice(cursor);
	return result;
}

async function renderMarkdown(source: string) {
	const parsed = parseFrontmatter(source);
	const headings: GeneratedHeading[] = [];
	const renderer = new marked.Renderer();

	renderer.heading = ({ tokens, depth }) => {
		const text = tokens
			.map((token) => ("text" in token ? token.text : ""))
			.join("")
			.trim();
		const id = slugifyHeading(text);
		headings.push({ depth, id, text });
		const inner = renderer.parser.parseInline(tokens);
		return `<h${depth} id="${id}"><a href="#${id}">${inner}</a></h${depth}>`;
	};

	const withSnippets = await replaceContentPlaceholders(parsed.body);
	const html = marked.parse(withSnippets, {
		gfm: true,
		renderer,
	}) as string;

	return {
		html,
		headings,
	};
}

async function renderMarkdownDocument(path: string) {
	const source = await readFile(path, "utf8");
	assertNoRawMusiFences(source, path);
	assertConsumerSafeDocs(source, path);
	assertRequiredSections(source, path);
	const parsed = parseFrontmatter(source);
	const attributes = parsed.attributes as Partial<MarkdownDocumentAttributes>;
	const rendered = await renderMarkdown(source);

	return {
		...(attributes as MarkdownDocumentAttributes),
		descriptionHtml: renderInlineHtml(String(attributes.description ?? "")),
		headings: rendered.headings,
		html: rendered.html,
		summaryHtml: renderInlineHtml(String(attributes.summary ?? "")),
	} satisfies GeneratedDoc;
}

export async function generateContent() {
	for (const snippet of contentSnippets) {
		validateSnippetSyntax(snippet.id, snippet.sourceText, snippet.language);
	}
	for (const example of exampleGroups) {
		for (const language of comparisonOrder) {
			if (!example.variants[language]) {
				throw new Error(`example group ${example.id} is missing ${language}`);
			}
		}
	}

	const renderedSnippets = {
		homeSampleHtml: await renderExample("option-fallback"),
		installSourceHtml: await renderSnippet("install-source"),
		quickstartHtml: await renderSnippet("quickstart"),
	};

	const docPaths = [
		"getting-started.md",
		"first-program.md",
		"files-packages-and-entry.md",
		"expressions-and-bindings.md",
		"functions-and-calls.md",
		"imports-and-packages.md",
		"data-and-pattern-matching.md",
		"records-arrays-and-mutation.md",
		"effects-and-handlers.md",
		"types.md",
		"classes-instances-and-laws.md",
		"attributes-and-foreign.md",
		"quote-and-syntax.md",
		"foundation-and-standard-library.md",
		"testing-and-running.md",
	];

	const renderedDocs = await Promise.all(
		docPaths.map((path) =>
			renderMarkdownDocument(join(appRoot, "src", "content", "docs", path)),
		),
	);

	const generated = `export interface GeneratedHeading {
\tdepth: number;
\tid: string;
\ttext: string;
}

export interface GeneratedDoc {
\ttitle: string;
\tdescription: string;
\tdescriptionHtml: string;
\tgroup: string;
\tsection: string;
\torder: number;
\tslug: string;
\tsummary: string;
\tsummaryHtml: string;
\theadings: GeneratedHeading[];
\thtml: string;
}

export const renderedSnippets = ${JSON.stringify(renderedSnippets, null, "\t")} as const;

export const renderedDocs = ${JSON.stringify(renderedDocs, null, "\t")} satisfies GeneratedDoc[];
`;

	await writeFile(generatedContentPath, generated, "utf8");
}
