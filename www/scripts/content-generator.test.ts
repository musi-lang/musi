import { readdirSync, readFileSync } from "node:fs";
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
const embedPattern = /\{\{(snippet|example):([\w-]+)\}\}/g;
const removedSyntaxPatterns = [
	/\bperform\b/,
	/\bcase\s+[^\n]+\bof\b/,
	/handle\s+request[^\n]+using[^\n]+\(/,
	/\.\{/,
] as const;
const requestKeywordPattern = /> ?request<\/span>/;
const matchKeywordPattern = /> ?match<\/span>/;
const partialKeywordPattern = /> ?partial<\/span>/;
const equivalenceOperatorPattern = /<span style="color:[^"]+">~=<\/span>/;
const pureFunctionOperatorPattern = /<span style="color:[^"]+">\s*-><\/span>/;
const payloadLabelPattern = /<span style="color:[^"]+">value<\/span>/;
const snippetPlaceholder = "${";
const matchSnippetPattern = `match ${snippetPlaceholder}1:expr} (`;
const handleRequestSnippetPattern = `handle ${snippetPlaceholder}1:effect}.${snippetPlaceholder}2:op}(${snippetPlaceholder}3:value}) using ${snippetPlaceholder}4:effect} {`;
const caseSnippetPattern = `case ${snippetPlaceholder}1:expr} of (`;
const handleWithSnippetPattern = `handle ${snippetPlaceholder}1:expr} with ${snippetPlaceholder}2:effect} of (`;

function chapterDocFiles(chaptersRoot: string) {
	const chapterFiles: string[] = [];
	for (const entry of readdirSync(chaptersRoot, { withFileTypes: true })) {
		if (entry.name === "index.md" || entry.name === "syntax.md") {
			continue;
		}
		const path = join(chaptersRoot, entry.name);
		if (entry.isDirectory()) {
			chapterFiles.push(...chapterDocFiles(path));
			continue;
		}
		if (entry.name.endsWith(".md")) {
			chapterFiles.push(path);
		}
	}
	return chapterFiles;
}

function chapterSnippetUsage(file: string) {
	const source = readFileSync(file, "utf8");
	const embeds = [...source.matchAll(embedPattern)];
	const snippetEmbeds = embeds.filter((embed) => embed[1] === "snippet");
	return { embeds, snippetEmbeds };
}

function removedSyntaxMatches(path: string) {
	if (!(path.endsWith(".md") || path.endsWith(".ts"))) {
		return [];
	}
	const source = readFileSync(path, "utf8");
	return removedSyntaxPatterns.filter((pattern) => pattern.test(source));
}

function visitContentPath(target: string, visitFile: (path: string) => void) {
	if (target.endsWith(".md") || target.endsWith(".ts")) {
		visitFile(target);
		return;
	}
	for (const entry of readdirSync(target, { withFileTypes: true })) {
		const path = join(target, entry.name);
		if (entry.isDirectory()) {
			visitContentPath(path, visitFile);
			continue;
		}
		visitFile(path);
	}
}

describe("content watch paths", () => {
	it("tracks docs, registries, grammar, and generator sources", () => {
		expect(
			isWatchedContentPath(
				join(
					root,
					"..",
					"..",
					"docs",
					"what",
					"language",
					"start",
					"first-program.md",
				),
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
		expect(
			isWatchedContentPath(join(root, "..", "src", "content", "catalog.ts")),
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
  | Configured(port : Int)
  | Default
};`,
			"musi",
		);
		const bindingHtml = renderHighlightedCodeForTest(
			"let port : Port := .Configured(port := 8080);",
			"musi",
		);

		expect(declarationHtml).toMatch(letTokenPattern);
		expect(bindingHtml).toMatch(plainValueBindingPattern);
		expect(bindingHtml).toContain(".Configured");
		expect(bindingHtml).not.toMatch(splitCallLikeBindingPattern);
	});
});

describe("chapter docs", () => {
	it("use registered snippets in every chapter", () => {
		const chaptersRoot = join(root, "..", "..", "docs", "what", "language");
		for (const file of chapterDocFiles(chaptersRoot)) {
			const usage = chapterSnippetUsage(file);
			expect(usage.snippetEmbeds.length, file).toBeGreaterThanOrEqual(1);
			expect(usage.snippetEmbeds[0]?.[1], file).toBe("snippet");
		}
	});
});

describe("website learning syntax", () => {
	it("avoid removed surface syntax in learning sources", () => {
		const files = [
			join(root, "..", "..", "docs", "what", "language"),
			join(root, "..", "src", "content", "snippet-registry.ts"),
			join(root, "..", "src", "content", "examples", "groups-core.ts"),
			join(root, "..", "src", "content", "examples", "groups-advanced.ts"),
		];
		for (const file of files) {
			visitContentPath(file, (path) => {
				for (const pattern of removedSyntaxMatches(path)) {
					expect(path, `${path} matches ${pattern}`).toBe("");
				}
			});
		}
	});
});

describe("website highlighting", () => {
	it("highlights request and match as keywords", () => {
		const html = renderHighlightedCodeForTest(
			`match value (
| .Left(x) => x
| .Right(_) => request Console.readLine()
);`,
			"musi",
		);

		expect(html).toContain("request");
		expect(html).toContain("match");
		expect(html).toMatch(requestKeywordPattern);
		expect(html).toMatch(matchKeywordPattern);
	});

	it("highlights dependent and type-constructor syntax", () => {
		const html = renderHighlightedCodeForTest(
			`let Box1[T] := data {
  | Box1(value : T)
};

let Keeps[F : Type -> Type] := class {
  let keep(value : F[Int]) : F[Int];
};

partial let parsePort(text : String) : Int := 0;
let proof := left ~= right;`,
			"musi",
		);

		expect(html).toContain("partial");
		expect(html).toContain("~=");
		expect(html).toContain("Type");
		expect(html).toContain("value");
		expect(html).toMatch(partialKeywordPattern);
		expect(html).toMatch(equivalenceOperatorPattern);
		expect(html).toMatch(pureFunctionOperatorPattern);
		expect(html).toMatch(payloadLabelPattern);
	});

	it("keeps grammar and snippets on current syntax", () => {
		const grammarSource = readFileSync(
			join(root, "..", "..", "vscode-ext", "syntaxes", "musi.tmLanguage.json"),
			"utf8",
		);
		const snippetSource = readFileSync(
			join(root, "..", "..", "vscode-ext", "snippets", "musi_snippets.json"),
			"utf8",
		);
		const websiteGeneratorSource = readFileSync(
			join(root, "content-generator.ts"),
			"utf8",
		);

		expect(grammarSource).toContain(
			"\\\\b(as|export|forall|handle|if|import|match|quote|request|resume|unsafe|where)\\\\b",
		);
		expect(grammarSource).not.toContain(
			"\\\\b(as|case|export|forall|handle|if|import|of|perform|quote|resume|where)\\\\b",
		);
		expect(grammarSource).toContain("match\\\\b");
		expect(grammarSource).toContain("partial");
		expect(grammarSource).toContain("request\\\\b");
		expect(grammarSource).toContain("~=");
		expect(grammarSource).not.toContain("case\\\\b");
		expect(grammarSource).not.toContain("of\\\\b");
		expect(grammarSource).not.toContain("perform\\\\b");
		expect(snippetSource).toContain('"Match Expression"');
		expect(snippetSource).toContain(matchSnippetPattern);
		expect(snippetSource).toContain(handleRequestSnippetPattern);
		expect(snippetSource).not.toContain(caseSnippetPattern);
		expect(snippetSource).not.toContain(handleWithSnippetPattern);
		expect(websiteGeneratorSource).toContain("match\\\\b");
		expect(websiteGeneratorSource).toContain("partial\\\\b");
		expect(websiteGeneratorSource).toContain("request\\\\b");
		expect(websiteGeneratorSource).not.toContain("case\\\\b");
		expect(websiteGeneratorSource).not.toContain("of\\\\b");
		expect(websiteGeneratorSource).not.toContain("perform\\\\b");
	});
});
