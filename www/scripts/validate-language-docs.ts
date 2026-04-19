import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import {
	bookPages,
	bookParts,
	bookSections,
} from "../src/content/book/manifest";
import { tryBlocks } from "../src/content/book/try-registry";
import { developerComparisons } from "../src/content/comparisons/developers";
import { exampleGroups } from "../src/content/examples/groups";
import { contentSnippets } from "../src/content/snippet-registry";

const repoRoot = join(import.meta.dir, "..", "..");
const rawPlaceholderPattern = /\{\{([^{}\n]+)\}\}/g;
const supportedPlaceholderPattern =
	/^\s*(snippet|example|try|compare):([\w-]+)\s*$/;
const markdownLinkPattern = /\[[^\]\n]+\]\(([^)\s]+)(?:\s+"[^"]*")?\)/g;
const rawMusiFencePattern = /```musi\b/i;
const integerPattern = /^-?\d+$/;
const leadingNewlinePattern = /^\n+/;
const frontmatterLinePattern = /^([A-Za-z][A-Za-z0-9_-]*):\s*(.*)$/;
const checkedInternalLinkPattern =
	/^\/(?:learn|docs)\/(?:book|language)\b|^\/learn\/guides\b/;

const requiredFrontmatterKeys = [
	"title",
	"description",
	"group",
	"section",
	"order",
	"slug",
	"summary",
] as const;

type RequiredFrontmatterKey = (typeof requiredFrontmatterKeys)[number];

export type FrontmatterValue = string | number;
export type MarkdownFrontmatter = Partial<
	Record<RequiredFrontmatterKey, FrontmatterValue>
>;

export interface ParsedMarkdownDoc {
	sourcePath: string;
	frontmatter: MarkdownFrontmatter;
	body: string;
}

export interface ValidateLanguageDocsResult {
	errors: string[];
	checkedFiles: number;
}

function parseFrontmatterValue(rawValue: string) {
	const trimmed = rawValue.trim();
	if (
		(trimmed.startsWith('"') && trimmed.endsWith('"')) ||
		(trimmed.startsWith("'") && trimmed.endsWith("'"))
	) {
		return trimmed.slice(1, -1);
	}
	if (integerPattern.test(trimmed)) {
		return Number.parseInt(trimmed, 10);
	}
	return trimmed;
}

export function parseMarkdownDoc(
	source: string,
	sourcePath: string,
): ParsedMarkdownDoc {
	const normalized = source.replaceAll("\r\n", "\n");
	if (!normalized.startsWith("---\n")) {
		throw new Error(`${sourcePath}: missing frontmatter opening fence`);
	}
	const frontmatterEndIndex = normalized.indexOf("\n---\n", 4);
	if (frontmatterEndIndex === -1) {
		throw new Error(`${sourcePath}: missing frontmatter closing fence`);
	}
	const frontmatterBlock = normalized.slice(4, frontmatterEndIndex);
	const body = normalized
		.slice(frontmatterEndIndex + 5)
		.replace(leadingNewlinePattern, "");
	const frontmatter: MarkdownFrontmatter = {};

	for (const line of frontmatterBlock.split("\n")) {
		if (line.trim().length === 0) {
			continue;
		}
		const match = line.match(frontmatterLinePattern);
		if (!match) {
			throw new Error(`${sourcePath}: invalid frontmatter line \`${line}\``);
		}
		const [, rawKey, rawValue] = match;
		const key = rawKey as RequiredFrontmatterKey;
		if (!requiredFrontmatterKeys.includes(key)) {
			continue;
		}
		frontmatter[key] = parseFrontmatterValue(rawValue);
	}

	return {
		sourcePath,
		frontmatter,
		body,
	};
}

export async function loadMarkdownDoc(
	sourcePath: string,
): Promise<ParsedMarkdownDoc> {
	const absolutePath = join(repoRoot, sourcePath);
	const source = await readFile(absolutePath, "utf8");
	return parseMarkdownDoc(source, sourcePath);
}

function collectDuplicateErrors(
	items: readonly { id: string }[],
	label: string,
	errors: string[],
) {
	const seen = new Set<string>();
	for (const item of items) {
		if (seen.has(item.id)) {
			errors.push(`${label}: duplicate id \`${item.id}\``);
			continue;
		}
		seen.add(item.id);
	}
}

function normalizePath(path: string) {
	if (path === "/") {
		return path;
	}
	return path.endsWith("/") ? path.slice(0, -1) : path;
}

function toLearnPath(path: string) {
	if (!path.startsWith("/docs/")) {
		return normalizePath(path);
	}
	return normalizePath(`/learn/${path.slice("/docs/".length)}`);
}

function toDocsPath(path: string) {
	if (!path.startsWith("/learn/")) {
		return normalizePath(path);
	}
	return normalizePath(`/docs/${path.slice("/learn/".length)}`);
}

function lastPathSegment(path: string) {
	const normalized = normalizePath(path);
	const index = normalized.lastIndexOf("/");
	return index === -1 ? normalized : normalized.slice(index + 1);
}

function addKnownPath(paths: Set<string>, path: string) {
	const normalized = normalizePath(path);
	paths.add(normalized);
	paths.add(toLearnPath(normalized));
	paths.add(toDocsPath(normalized));
}

function addKnownPathSet(paths: Set<string>, pathsToAdd: readonly string[]) {
	for (const path of pathsToAdd) {
		addKnownPath(paths, path);
	}
}

function buildKnownInternalDocPaths() {
	const paths = new Set<string>();
	const sectionPathById = new Map<string, string>();

	for (const part of bookParts) {
		addKnownPathSet(paths, [part.path, ...(part.aliases ?? [])]);
	}

	for (const section of bookSections) {
		const sectionPath = toLearnPath(section.path);
		sectionPathById.set(section.id, sectionPath);
		addKnownPathSet(paths, [section.path, ...(section.aliases ?? [])]);
	}

	for (const page of bookPages) {
		const basePath = toLearnPath(page.path);
		const sectionPath = page.sectionId
			? sectionPathById.get(page.sectionId)
			: null;
		const canonicalPath =
			sectionPath && !basePath.startsWith(`${sectionPath}/`)
				? `${sectionPath}/${lastPathSegment(basePath)}`
				: basePath;
		addKnownPathSet(paths, [canonicalPath, page.path, ...page.aliases]);
	}

	return paths;
}

function validateFrontmatter(
	doc: ParsedMarkdownDoc,
	entryId: string,
	errors: string[],
) {
	for (const key of requiredFrontmatterKeys) {
		const value = doc.frontmatter[key];
		if (value === undefined) {
			errors.push(
				`${doc.sourcePath}: missing frontmatter key \`${key}\` for \`${entryId}\``,
			);
			continue;
		}
		if (key === "order") {
			if (typeof value !== "number" || !Number.isInteger(value)) {
				errors.push(
					`${doc.sourcePath}: frontmatter \`order\` must be integer for \`${entryId}\``,
				);
			}
			continue;
		}
		if (typeof value !== "string" || value.trim().length === 0) {
			errors.push(
				`${doc.sourcePath}: frontmatter \`${key}\` must be non-empty string for \`${entryId}\``,
			);
		}
	}
}

function validateSourceContent(
	doc: ParsedMarkdownDoc,
	entryId: string,
	snippetIds: ReadonlySet<string>,
	exampleIds: ReadonlySet<string>,
	tryBlockIds: ReadonlySet<string>,
	comparisonIds: ReadonlySet<string>,
	errors: string[],
) {
	if (rawMusiFencePattern.test(doc.body)) {
		errors.push(
			`${doc.sourcePath}: raw \`\`\`musi fence found for \`${entryId}\``,
		);
	}

	for (const match of doc.body.matchAll(rawPlaceholderPattern)) {
		const rawPayload = match[1] ?? "";
		const parsed = rawPayload.match(supportedPlaceholderPattern);
		if (!parsed) {
			errors.push(
				`${doc.sourcePath}: unsupported placeholder \`{{${rawPayload}}}\` for \`${entryId}\``,
			);
			continue;
		}
		const [, kind, referenceId] = parsed;
		const isKnownReference =
			(kind === "snippet" && snippetIds.has(referenceId)) ||
			(kind === "example" && exampleIds.has(referenceId)) ||
			(kind === "try" && tryBlockIds.has(referenceId)) ||
			(kind === "compare" && comparisonIds.has(referenceId));
		if (isKnownReference) {
			continue;
		}
		errors.push(
			`${doc.sourcePath}: unknown ${kind} placeholder id \`${referenceId}\` for \`${entryId}\``,
		);
	}
}

function validateInternalLinks(
	doc: ParsedMarkdownDoc,
	entryId: string,
	internalDocPaths: ReadonlySet<string>,
	errors: string[],
) {
	for (const match of doc.body.matchAll(markdownLinkPattern)) {
		const rawHref = match[1] ?? "";
		if (!checkedInternalLinkPattern.test(rawHref)) {
			continue;
		}
		const [rawPath = ""] = rawHref.split("#", 1);
		const linkPath = normalizePath(rawPath);
		if (internalDocPaths.has(linkPath)) {
			continue;
		}
		errors.push(
			`${doc.sourcePath}: unknown internal docs link \`${rawHref}\` for \`${entryId}\``,
		);
	}
}

export async function validateLanguageDocs(): Promise<ValidateLanguageDocsResult> {
	const errors: string[] = [];
	const checkedFiles = new Set<string>();

	collectDuplicateErrors(bookParts, "bookParts", errors);
	collectDuplicateErrors(bookSections, "bookSections", errors);
	collectDuplicateErrors(bookPages, "bookPages", errors);
	collectDuplicateErrors(contentSnippets, "contentSnippets", errors);
	collectDuplicateErrors(exampleGroups, "exampleGroups", errors);
	collectDuplicateErrors(tryBlocks, "tryBlocks", errors);
	collectDuplicateErrors(developerComparisons, "developerComparisons", errors);

	const snippetIds = new Set(contentSnippets.map((snippet) => snippet.id));
	const exampleIds = new Set(exampleGroups.map((group) => group.id));
	const tryBlockIds = new Set(tryBlocks.map((block) => block.id));
	const comparisonIds = new Set(
		developerComparisons.map((comparison) => comparison.id),
	);
	const internalDocPaths = buildKnownInternalDocPaths();

	for (const page of bookPages) {
		if (!page.sourcePath.startsWith("docs/what/language/")) {
			errors.push(
				`bookPages: \`${page.id}\` source path must stay in docs/what/language (found \`${page.sourcePath}\`)`,
			);
		}
	}

	const docEntries = [
		...bookParts.map((part) => ({ id: part.id, sourcePath: part.sourcePath })),
		...bookSections
			.map((section) => {
				const sourcePath =
					"sourcePath" in section && typeof section.sourcePath === "string"
						? section.sourcePath
						: null;
				return sourcePath ? { id: section.id, sourcePath } : null;
			})
			.filter(
				(
					entry,
				): entry is {
					id: string;
					sourcePath: string;
				} => entry !== null && !entry.sourcePath.startsWith("generated://"),
			),
		...bookPages.map((page) => ({ id: page.id, sourcePath: page.sourcePath })),
	];

	for (const entry of docEntries) {
		const absolutePath = join(repoRoot, entry.sourcePath);
		if (!existsSync(absolutePath)) {
			errors.push(`${entry.sourcePath}: file missing for \`${entry.id}\``);
			continue;
		}
		const doc = await loadMarkdownDoc(entry.sourcePath);
		validateFrontmatter(doc, entry.id, errors);
		validateSourceContent(
			doc,
			entry.id,
			snippetIds,
			exampleIds,
			tryBlockIds,
			comparisonIds,
			errors,
		);
		validateInternalLinks(doc, entry.id, internalDocPaths, errors);
		checkedFiles.add(entry.sourcePath);
	}

	errors.sort((left, right) => left.localeCompare(right));
	return {
		errors,
		checkedFiles: checkedFiles.size,
	};
}

async function main() {
	const result = await validateLanguageDocs();
	if (result.errors.length > 0) {
		for (const error of result.errors) {
			console.error(error);
		}
		process.exitCode = 1;
		return;
	}
	console.log(
		`language docs validation passed (${result.checkedFiles} markdown files)`,
	);
}

if (import.meta.main) {
	main().catch((error) => {
		console.error(error);
		process.exitCode = 1;
	});
}
