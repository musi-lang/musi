import { readFile, writeFile } from "node:fs/promises";
import { dirname, join, normalize, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { marked } from "marked";
import { createHighlighter } from "shiki";
import {
	bookPages,
	bookParts,
	bookSections,
} from "../src/content/book/manifest";
import { tryBlockById } from "../src/content/book/try-registry";
import { exampleGroupById } from "../src/content/examples/groups";
import { contentSnippets, snippetById } from "../src/content/snippet-registry";
import type { Locale } from "../src/lib/site-copy";

interface TextMateRule {
	include?: string;
	name?: string;
	match?: string;
	patterns?: TextMateRule[];
	begin?: string;
	end?: string;
	beginCaptures?: Record<string, { name: string }>;
	endCaptures?: Record<string, { name: string }>;
	captures?: Record<string, { name: string }>;
}

interface TextMateGrammar {
	name?: string;
	scopeName?: string;
	patterns?: TextMateRule[];
	repository?: Record<string, TextMateRule>;
}

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
	locale: Locale;
	id: string;
	kind: "part" | "section" | "chapter";
	parentId: string | null;
	depth: number;
	treePath: string[];
	childIds: string[];
	partId: string;
	partTitle: string;
	sectionId: string | null;
	sectionTitle: string | null;
	path: string;
	canonicalPath: string;
	aliases: string[];
	questions: { label: string; href: string }[];
	descriptionHtml: string;
	headings: GeneratedHeading[];
	html: string;
	summaryHtml: string;
}

const quotedValuePattern = /^"(.*)"$/;
const docsRoutePattern = /^\/docs/;

const numberValuePattern = /^\d+$/;
const rawMusiFencePattern = /```musi\b/;
const nonAsciiQuotePattern = /[‘’“”]/;
const examplePattern = /\{\{example:([\w-]+)\}\}/g;
const snippetPattern = /\{\{snippet:([\w-]+)\}\}/g;
const tryPattern = /\{\{try:([\w-]+)\}\}/g;
const bannedSyntaxPatterns = [/\bif\b/, /\bthen\b/, /\belse\b/, /==/];
const stdlibRedefinitionPattern = /let\s+(Option|Result)\[[^\]]+\]\s*:=/;
const bannedDocsPatterns = [
	/current tests prove/i,
	/long-term design/i,
	/semantic collection/i,
	/compiler architecture/i,
	/current design/i,
	/@std\/io/,
	/old syntax/i,
	/new syntax/i,
	/important cleanup/i,
	/formerly/i,
	/used to/i,
	/current surface/i,
	/current shape/i,
	/in this chapter/i,
	/why it matters/i,
	/walk through it/i,
	/try it next/i,
];
const scriptsDirectory = dirname(fileURLToPath(import.meta.url));
const appRoot = join(scriptsDirectory, "..");
const repoRoot = join(appRoot, "..");
const docsDirectory = join(repoRoot, "docs", "what", "language");
const examplesDirectory = join(appRoot, "src", "content", "examples");
const bookContentDirectory = join(appRoot, "src", "content", "book");
const snippetRegistryPath = join(
	appRoot,
	"src",
	"content",
	"snippet-registry.ts",
);
const bookManifestPath = join(appRoot, "src", "content", "book", "manifest.ts");
const contentCatalogPath = join(appRoot, "src", "content", "catalog.ts");
const tryRegistryPath = join(
	appRoot,
	"src",
	"content",
	"book",
	"try-registry.ts",
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

function createWebsiteMusiGrammar(
	rawGrammar: TextMateGrammar,
): TextMateGrammar {
	const grammar = structuredClone(rawGrammar);
	const repository = grammar.repository ?? {};
	const rootPatterns = grammar.patterns ?? [];

	repository["website-function-definition"] = {
		match:
			"\\b(let)\\s+(?:(rec)\\s+)?([a-z_][A-Za-z0-9_]*)\\b(?=\\s*(?:\\[[^\\]\\n]*\\]\\s*)?\\()",
		captures: {
			"1": { name: "keyword.declaration.musi" },
			"2": { name: "keyword.modifier.musi" },
			"3": { name: "entity.name.function.definition.musi" },
		},
	};

	repository["website-type-definition"] = {
		match:
			"\\b(let)\\s+(?:(rec)\\s+)?([A-Z][A-Za-z0-9_]*)\\b(?=\\s*:=\\s*(?:class|data|effect|instance)\\b)",
		captures: {
			"1": { name: "keyword.declaration.musi" },
			"2": { name: "keyword.modifier.musi" },
			"3": { name: "support.type.identifier.musi" },
		},
	};

	repository["website-value-definition"] = {
		match: "\\b(let)\\s+(?:(rec)\\s+)?([a-z_][A-Za-z0-9_]*)\\b(?=\\s*(?::|:=))",
		captures: {
			"1": { name: "keyword.declaration.musi" },
			"2": { name: "keyword.modifier.musi" },
			"3": { name: "variable.other.definition.musi" },
		},
	};

	repository["website-function-call"] = {
		name: "entity.name.function.call.musi",
		match:
			"(?<!\\.)\\b(?!and\\b|as\\b|class\\b|data\\b|effect\\b|export\\b|foreign\\b|forall\\b|handle\\b|if\\b|import\\b|in\\b|infix\\b|infixl\\b|infixr\\b|instance\\b|law\\b|let\\b|match\\b|mut\\b|not\\b|opaque\\b|or\\b|partial\\b|quote\\b|rec\\b|request\\b|resume\\b|shl\\b|shr\\b|unsafe\\b|using\\b|where\\b|with\\b|xor\\b)[a-z_][A-Za-z0-9_]*\\b(?=\\s*\\()",
	};

	repository["website-type-identifier"] = {
		name: "support.type.identifier.musi",
		match: "\\b[A-Z][A-Za-z0-9_]*\\b",
	};

	repository["website-constructor-variant"] = {
		name: "support.type.identifier.musi",
		match: "\\.[A-Z][A-Za-z0-9_]*\\b",
	};

	const typeAnnotationRule = repository["type-annotation"];
	if (typeAnnotationRule?.patterns) {
		typeAnnotationRule.patterns = [
			{ include: "#website-type-identifier" },
			...typeAnnotationRule.patterns,
		];
	}

	grammar.repository = repository;
	grammar.patterns = [
		{ include: "#website-type-definition" },
		{ include: "#website-function-definition" },
		{ include: "#website-value-definition" },
		{ include: "#website-constructor-variant" },
		{ include: "#website-function-call" },
		{ include: "#website-type-identifier" },
		...rootPatterns,
	];

	return grammar;
}

export const generatedContentPath = join(
	appRoot,
	"src",
	"generated-content.ts",
);

export const watchedContentPaths = [
	docsDirectory,
	examplesDirectory,
	bookContentDirectory,
	snippetRegistryPath,
	bookManifestPath,
	contentCatalogPath,
	tryRegistryPath,
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
		normalized.startsWith(pathWithTrailingSeparator(examplesDirectory)) ||
		normalized.startsWith(pathWithTrailingSeparator(bookContentDirectory)) ||
		normalized === normalize(snippetRegistryPath) ||
		normalized === normalize(bookManifestPath) ||
		normalized === normalize(contentCatalogPath) ||
		normalized === normalize(tryRegistryPath) ||
		normalized === normalize(generatorModulePath) ||
		normalized === normalize(generatorEntrypointPath) ||
		normalized === normalize(grammarPath)
	);
}

const highlighter = await createHighlighter({
	themes: ["github-light-high-contrast", "github-dark"],
	langs: [
		"bash",
		"c",
		"css",
		"csharp",
		"cpp",
		"fsharp",
		"go",
		"html",
		"javascript",
		"json",
		"java",
		"kotlin",
		"markdown",
		"plaintext",
		"python",
		"rust",
		"scala",
		"toml",
		"typescript",
		"xml",
		"yaml",
		{
			...createWebsiteMusiGrammar(
				JSON.parse(await readFile(grammarPath, "utf8")) as TextMateGrammar,
			),
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
		case "js":
			return "javascript";
		case "ts":
			return "typescript";
		case "md":
			return "markdown";
		case "markup":
			return "html";
		case "yml":
			return "yaml";
		case "text":
			return "plaintext";
		case "scala3":
			return "scala";
		default:
			return language;
	}
}

function renderHighlightedCode(sourceText: string, language: string) {
	const normalizedLanguage = normalizeLanguage(language);
	try {
		return highlighter.codeToHtml(sourceText, {
			lang: normalizedLanguage,
			themes: {
				light: "github-light-high-contrast",
				dark: "github-dark",
			},
		});
	} catch (error) {
		if (normalizedLanguage === "musi") {
			const cause = error instanceof Error ? error.message : String(error);
			throw new Error(`musi highlighting failed: ${cause}`);
		}
		return highlighter.codeToHtml(sourceText, {
			lang: "plaintext",
			themes: {
				light: "github-light-high-contrast",
				dark: "github-dark",
			},
		});
	}
}

export function renderHighlightedCodeForTest(
	sourceText: string,
	language: string,
) {
	return renderHighlightedCode(sourceText, language);
}

function slugifyHeading(text: string) {
	return text
		.toLowerCase()
		.replace(/[`']/g, "")
		.replace(/[^\p{Letter}\p{Number}]+/gu, "-")
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
	if (nonAsciiQuotePattern.test(source)) {
		throw new Error(`${path} contains non-ASCII quote punctuation`);
	}

	for (const pattern of bannedDocsPatterns) {
		if (pattern.test(source)) {
			throw new Error(`${path} contains banned docs content: ${pattern}`);
		}
	}
}

function docSourcePath(path: string) {
	return join(repoRoot, path);
}

function renderSnippet(id: string) {
	const snippet = snippetById(id);
	if (!snippet) {
		throw new Error(`missing snippet ${id}`);
	}

	validateSnippetSyntax(snippet.id, snippet.sourceText, snippet.language);
	const html = renderHighlightedCode(snippet.sourceText, snippet.language);
	return `<div class="snippet-block"><section class="code-panel">${html}</section></div>`;
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

function renderTryBlock(id: string) {
	const block = tryBlockById(id);
	if (!block) {
		throw new Error(`missing try block ${id}`);
	}

	const steps = block.steps
		.map((step) => `<li>${renderInlineHtml(step)}</li>`)
		.join("");

	return `<div class="try-block"><ol>${steps}</ol></div>`;
}

function renderExample(id: string) {
	const group = exampleGroupById(id);
	if (!group) {
		throw new Error(`missing example group ${id}`);
	}

	validateSnippetSyntax(`${id}:musi`, group.sourceText, "musi");

	const html = renderHighlightedCode(group.sourceText, "musi");

	return `<div class="code-tabs" data-example-id="${escapeHtmlAttribute(id)}"><div class="code-tabs-meta"><p class="code-tabs-caption">${renderInlineHtml(group.caption)}</p><p class="code-tabs-note">${renderInlineHtml(group.note)}</p></div><section role="tabpanel" class="code-panel">${html}</section></div>`;
}

function replaceContentPlaceholders(source: string) {
	const snippetMatches = Array.from(source.matchAll(snippetPattern));
	const exampleMatches = Array.from(source.matchAll(examplePattern));
	const tryMatches = Array.from(source.matchAll(tryPattern));
	const matches = [...snippetMatches, ...exampleMatches, ...tryMatches].sort(
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
		if (matched.startsWith("{{example:")) {
			result += renderExample(id);
		} else if (matched.startsWith("{{try:")) {
			result += renderTryBlock(id);
		} else {
			result += renderSnippet(id);
		}
		cursor = start + matched.length;
	}

	result += source.slice(cursor);
	return result;
}

function renderMarkdown(source: string) {
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

	renderer.code = ({ text, lang }) => {
		const language =
			typeof lang === "string" && lang.length > 0 ? lang : "plaintext";
		return renderHighlightedCode(text, language);
	};

	const withSnippets = replaceContentPlaceholders(parsed.body);
	const html = marked.parse(withSnippets, {
		gfm: true,
		renderer,
	}) as string;

	return {
		html,
		headings,
	};
}

function setDocRewrite(
	rewrites: Map<string, string>,
	from: string,
	to: string,
) {
	const localizedFrom = localizedDocRoute("en", from);
	const localizedTo = localizedDocRoute("en", to);
	rewrites.set(from, localizedTo);
	rewrites.set(localizedFrom, localizedTo);
}

function buildDocLinkMap() {
	const rewrites = new Map<string, string>();
	const sectionById = new Map(
		bookSections.map((section) => [section.id, section]),
	);
	for (const part of bookParts) {
		setDocRewrite(rewrites, part.path, part.path);
		for (const alias of part.aliases ?? []) {
			setDocRewrite(rewrites, alias, part.path);
		}
	}
	for (const section of bookSections) {
		setDocRewrite(rewrites, section.path, section.path);
		for (const alias of section.aliases ?? []) {
			setDocRewrite(rewrites, alias, section.path);
		}
	}
	for (const page of bookPages) {
		const section = sectionById.get(page.sectionId);
		if (!section) {
			throw new Error(`missing section ${page.sectionId} for page ${page.id}`);
		}
		const chapterPath = chapterPathForSection(section.path, page.path);
		setDocRewrite(rewrites, page.path, chapterPath);
		for (const alias of page.aliases) {
			setDocRewrite(rewrites, alias, chapterPath);
		}
	}
	return rewrites;
}

function rewriteDocLinks(source: string, rewrites: Map<string, string>) {
	return source.replaceAll(
		/\]\(((?:\/docs|\/learn)\/[^)#\s]+)\)/g,
		(matched, path) => {
			const replacement = rewrites.get(path);
			return replacement ? matched.replace(path, replacement) : matched;
		},
	);
}

function localizeMarkdownLinks(_locale: Locale, source: string) {
	return source;
}

function localizedDocRoute(_locale: Locale, path: string) {
	return path.replace(docsRoutePattern, "/learn");
}

function lastPathSegment(path: string) {
	const segments = path.split("/").filter(Boolean);
	const last = segments.at(-1);
	if (!last) {
		throw new Error(`missing path segment for ${path}`);
	}
	return last;
}

function chapterPathForSection(sectionPath: string, chapterPath: string) {
	return `${sectionPath}/${lastPathSegment(chapterPath)}`;
}

async function renderMarkdownDocument(input: {
	locale: Locale;
	id: string;
	kind: "part" | "section" | "chapter";
	parentId: string | null;
	depth: number;
	treePath: string[];
	childIds: string[];
	partId: string;
	partTitle: string;
	sectionId: string | null;
	sectionTitle: string | null;
	path: string;
	aliases: string[];
	sourcePath?: string;
	staticAttributes?: MarkdownDocumentAttributes;
	questions: { label: string; href: string }[];
	rewrites: Map<string, string>;
}) {
	let attributes: Partial<MarkdownDocumentAttributes> =
		input.staticAttributes ?? {};
	let rendered: { html: string; headings: GeneratedHeading[] };

	if (input.sourcePath) {
		const source = await readFile(docSourcePath(input.sourcePath), "utf8");
		assertNoRawMusiFences(source, input.sourcePath);
		assertConsumerSafeDocs(source, input.sourcePath);
		const rewrittenSource = localizeMarkdownLinks(
			input.locale,
			rewriteDocLinks(source, input.rewrites),
		);
		const parsed = parseFrontmatter(rewrittenSource);
		attributes = {
			...attributes,
			...(parsed.attributes as Partial<MarkdownDocumentAttributes>),
		};
		rendered = renderMarkdown(rewrittenSource);
	} else {
		const description = String(attributes?.description ?? "");
		const summary = String(attributes?.summary ?? "");
		rendered = {
			html: `<p>${renderInlineHtml(description)}</p>`,
			headings: [],
		};
		attributes = {
			...attributes,
			description,
			summary,
		};
	}

	return {
		locale: input.locale,
		id: input.id,
		kind: input.kind,
		parentId: input.parentId,
		depth: input.depth,
		treePath: input.treePath,
		childIds: input.childIds,
		partId: input.partId,
		partTitle: input.partTitle,
		sectionId: input.sectionId,
		sectionTitle: input.sectionTitle,
		path: input.path,
		canonicalPath: input.path,
		aliases: input.aliases,
		questions: input.questions,
		...(attributes as MarkdownDocumentAttributes),
		descriptionHtml: renderInlineHtml(String(attributes.description ?? "")),
		headings: rendered.headings,
		html: rendered.html,
		summaryHtml: renderInlineHtml(String(attributes.summary ?? "")),
	} satisfies GeneratedDoc;
}

type ManifestSection = (typeof bookSections)[number];
type ManifestPage = (typeof bookPages)[number];

function compareByOrderThenTitle(
	left: { order: number; title: string },
	right: { order: number; title: string },
) {
	if (left.order !== right.order) {
		return left.order - right.order;
	}
	return left.title.localeCompare(right.title);
}

function buildSectionChildrenByParentId(
	sectionById: Map<string, ManifestSection>,
) {
	const sectionChildrenByParentId = new Map<string, ManifestSection[]>();
	for (const section of bookSections) {
		if (section.parentId) {
			const parentSection = sectionById.get(section.parentId);
			if (!parentSection) {
				throw new Error(
					`missing parent section ${section.parentId} for section ${section.id}`,
				);
			}
			if (parentSection.partId !== section.partId) {
				throw new Error(
					`mismatched part ${section.partId} for parent section ${parentSection.id} on section ${section.id}`,
				);
			}
		}
		const parentId = section.parentId ?? section.partId;
		const siblings = sectionChildrenByParentId.get(parentId) ?? [];
		siblings.push(section);
		sectionChildrenByParentId.set(parentId, siblings);
	}
	for (const sections of sectionChildrenByParentId.values()) {
		sections.sort(compareByOrderThenTitle);
	}
	return sectionChildrenByParentId;
}

function buildPagesBySectionId() {
	const pagesBySectionId = new Map<string, ManifestPage[]>();
	for (const page of bookPages) {
		const pages = pagesBySectionId.get(page.sectionId) ?? [];
		pages.push(page);
		pagesBySectionId.set(page.sectionId, pages);
	}
	return pagesBySectionId;
}

function partForSection(
	partById: Map<string, GeneratedDoc>,
	section: ManifestSection,
) {
	const part = partById.get(section.partId);
	if (!part) {
		throw new Error(`missing part ${section.partId} for section ${section.id}`);
	}
	return part;
}

function parentForSection(
	section: ManifestSection,
	part: GeneratedDoc,
	localizedSectionById: Map<string, GeneratedDoc>,
) {
	if (!section.parentId) {
		return part;
	}
	const parent = localizedSectionById.get(section.parentId);
	if (!parent) {
		throw new Error(
			`missing rendered parent ${section.parentId} for section ${section.id}`,
		);
	}
	return parent;
}

function sectionIdsForParent(
	sectionChildrenByParentId: Map<string, ManifestSection[]>,
	parentId: string,
) {
	return (sectionChildrenByParentId.get(parentId) ?? []).map(
		(section) => section.id,
	);
}

function pageIdsForSection(
	pagesBySectionId: Map<string, ManifestPage[]>,
	sectionId: string,
) {
	return (pagesBySectionId.get(sectionId) ?? []).map((page) => page.id);
}

function queueRootSections(
	localizedPartDocs: GeneratedDoc[],
	sectionChildrenByParentId: Map<string, ManifestSection[]>,
) {
	const queuedSections: ManifestSection[] = [];
	for (const part of localizedPartDocs) {
		for (const section of sectionChildrenByParentId.get(part.id) ?? []) {
			queuedSections.push(section);
		}
	}
	return queuedSections;
}

function queueChildSections(
	queuedSections: ManifestSection[],
	sectionChildrenByParentId: Map<string, ManifestSection[]>,
	sectionId: string,
) {
	for (const child of sectionChildrenByParentId.get(sectionId) ?? []) {
		queuedSections.push(child);
	}
}

function renderLocalizedSectionDoc(input: {
	locale: Locale;
	localizedAliases: (paths: string[]) => string[];
	rewrites: Map<string, string>;
	section: ManifestSection;
	part: GeneratedDoc;
	parent: GeneratedDoc;
	childIds: string[];
}) {
	return renderMarkdownDocument({
		locale: input.locale,
		id: input.section.id,
		kind: "section",
		parentId: input.parent.id,
		depth: input.parent.depth + 1,
		treePath: [...input.parent.treePath, input.section.id],
		childIds: input.childIds,
		partId: input.part.id,
		partTitle: input.part.title,
		sectionId: input.section.id,
		sectionTitle: input.section.title,
		path: localizedDocRoute(input.locale, input.section.path),
		aliases:
			input.locale === "en"
				? input.localizedAliases([
						input.section.path,
						...(input.section.aliases ?? []),
					])
				: [],
		...(input.section.sourcePath
			? { sourcePath: input.section.sourcePath }
			: {}),
		staticAttributes: {
			title: input.section.title,
			description: input.section.description,
			group: input.section.group,
			section: input.section.section,
			order: input.section.order,
			slug: input.section.slug,
			summary: input.section.summary,
		},
		questions: [],
		rewrites: input.rewrites,
	});
}

async function renderLocalizedSectionDocs(input: {
	locale: Locale;
	localizedAliases: (paths: string[]) => string[];
	rewrites: Map<string, string>;
	localizedPartDocs: GeneratedDoc[];
	partById: Map<string, GeneratedDoc>;
	sectionChildrenByParentId: Map<string, ManifestSection[]>;
	pagesBySectionId: Map<string, ManifestPage[]>;
}) {
	for (const part of input.localizedPartDocs) {
		part.childIds = (input.sectionChildrenByParentId.get(part.id) ?? []).map(
			(section) => section.id,
		);
	}
	const localizedSectionDocs: GeneratedDoc[] = [];
	const localizedSectionById = new Map<string, GeneratedDoc>();
	const queuedSections = queueRootSections(
		input.localizedPartDocs,
		input.sectionChildrenByParentId,
	);

	while (queuedSections.length > 0) {
		const section = queuedSections.shift() as ManifestSection;
		const part = partForSection(input.partById, section);
		const parent = parentForSection(section, part, localizedSectionById);
		const sectionDoc = await renderLocalizedSectionDoc({
			locale: input.locale,
			localizedAliases: input.localizedAliases,
			rewrites: input.rewrites,
			section,
			part,
			parent,
			childIds: [
				...sectionIdsForParent(input.sectionChildrenByParentId, section.id),
				...pageIdsForSection(input.pagesBySectionId, section.id),
			],
		});
		localizedSectionDocs.push(sectionDoc);
		localizedSectionById.set(section.id, sectionDoc);
		queueChildSections(
			queuedSections,
			input.sectionChildrenByParentId,
			section.id,
		);
	}

	return { localizedSectionDocs, localizedSectionById };
}

function renderLocalizedPageDocs(input: {
	locale: Locale;
	localizedAliases: (paths: string[]) => string[];
	rewrites: Map<string, string>;
	partById: Map<string, GeneratedDoc>;
	sectionById: Map<string, ManifestSection>;
	localizedSectionById: Map<string, GeneratedDoc>;
}) {
	return Promise.all(
		bookPages.map((page) => {
			const part = input.partById.get(page.partId);
			if (!part) {
				throw new Error(`missing part ${page.partId} for page ${page.id}`);
			}
			const section = input.sectionById.get(page.sectionId);
			if (!section) {
				throw new Error(
					`missing section ${page.sectionId} for page ${page.id}`,
				);
			}
			if (section.partId !== page.partId) {
				throw new Error(
					`mismatched part ${page.partId} for section ${section.id} on page ${page.id}`,
				);
			}
			const localizedSection = input.localizedSectionById.get(section.id);
			if (!localizedSection) {
				throw new Error(`missing localized section ${section.id}`);
			}
			const chapterPath = chapterPathForSection(section.path, page.path);
			return renderMarkdownDocument({
				locale: input.locale,
				id: page.id,
				kind: "chapter",
				parentId: section.id,
				depth: localizedSection.depth + 1,
				treePath: [...localizedSection.treePath, page.id],
				childIds: [],
				partId: page.partId,
				partTitle: part.title,
				sectionId: section.id,
				sectionTitle: localizedSection.title,
				path: localizedDocRoute(input.locale, chapterPath),
				aliases:
					input.locale === "en"
						? input.localizedAliases([chapterPath, page.path, ...page.aliases])
						: [],
				sourcePath: page.sourcePath,
				questions: [],
				rewrites: input.rewrites,
			});
		}),
	);
}

export async function generateContent() {
	for (const snippet of contentSnippets) {
		validateSnippetSyntax(snippet.id, snippet.sourceText, snippet.language);
	}

	const renderedSnippets = {
		homeSampleHtml: renderExample("home-intro"),
		installCurlHtml: renderSnippet("install-curl"),
		installPowershellHtml: renderSnippet("install-powershell"),
		installCargoHtml: renderSnippet("install-cargo"),
		quickstartHtml: renderSnippet("quickstart"),
	};

	const rewrites = buildDocLinkMap();
	const renderedDocs: GeneratedDoc[] = [];
	for (const locale of ["en"] as const) {
		const dedupePaths = (paths: string[]) => [...new Set(paths)];
		const localizedAliases = (paths: string[]) =>
			dedupePaths(
				paths.flatMap((path) => [path, localizedDocRoute(locale, path)]),
			);

		const localizedPartDocs = await Promise.all(
			bookParts.map((part) =>
				renderMarkdownDocument({
					locale,
					id: part.id,
					kind: "part",
					parentId: null,
					depth: 0,
					treePath: [part.id],
					childIds: [],
					partId: part.id,
					partTitle: part.id,
					sectionId: null,
					sectionTitle: null,
					path: localizedDocRoute(locale, part.path),
					aliases:
						locale === "en"
							? localizedAliases([part.path, ...(part.aliases ?? [])])
							: [],
					sourcePath: part.sourcePath,
					questions: [],
					rewrites,
				}),
			),
		);
		const partById = new Map(
			localizedPartDocs.map((part) => [part.id, part] as const),
		);
		const sectionById = new Map(
			bookSections.map((section) => [section.id, section] as const),
		);
		const sectionChildrenByParentId =
			buildSectionChildrenByParentId(sectionById);
		const pagesBySectionId = buildPagesBySectionId();
		const { localizedSectionDocs, localizedSectionById } =
			await renderLocalizedSectionDocs({
				locale,
				localizedAliases,
				rewrites,
				localizedPartDocs,
				partById,
				sectionChildrenByParentId,
				pagesBySectionId,
			});
		const localizedPageDocs = await renderLocalizedPageDocs({
			locale,
			localizedAliases,
			rewrites,
			partById,
			sectionById,
			localizedSectionById,
		});
		renderedDocs.push(
			...localizedPartDocs,
			...localizedSectionDocs,
			...localizedPageDocs,
		);
	}

	const generated = `export interface GeneratedHeading {
\tdepth: number;
\tid: string;
\ttext: string;
}

export interface GeneratedDoc {
\tlocale: "en";
\tid: string;
\tkind: "part" | "section" | "chapter";
\tparentId: string | null;
\tdepth: number;
\ttreePath: string[];
\tchildIds: string[];
\tpartId: string;
\tpartTitle: string;
\tsectionId: string | null;
\tsectionTitle: string | null;
\tpath: string;
\tcanonicalPath: string;
\taliases: string[];
\tquestions: { label: string; href: string }[];
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
