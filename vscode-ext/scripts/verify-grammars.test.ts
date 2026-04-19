import { expect, test } from "bun:test";
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { join, resolve } from "node:path";
import type { IGrammar, IToken, StateStack } from "vscode-textmate";

type GrammarFixtures = {
	requiredKeywordPatterns: string[];
	requiredOperatorPatterns: string[];
	requiredRepositoryKeys?: string[];
	requiredScopePatterns?: string[];
	requiredDocumentationPatterns?: string[];
	documentationCommentSamples?: string[];
	requiredAccessorScope: string;
	forbiddenRepositoryKeys?: string[];
	forbiddenScopePatterns?: string[];
	requiredSnippetPrefixes: string[];
	tokenFixtures?: TokenFixture[];
};

type Snippet = {
	prefix: string;
};

type TokenFixture = {
	name: string;
	source: string;
	checks?: TokenCheck[];
};

type TokenCheck = {
	line: number;
	lexeme: string;
	occurrence?: number;
	scope: string;
	forbiddenScopes?: string[];
};

const root = resolve(__dirname, "..");
const requireFromScript = createRequire(__filename);

function readJson<T>(relativePath: string): T {
	return JSON.parse(readFileSync(join(root, relativePath), "utf8")) as T;
}

function collectStrings(value: unknown, out: string[] = []): string[] {
	if (typeof value === "string") {
		out.push(value);
		return out;
	}
	if (Array.isArray(value)) {
		for (const entry of value) {
			collectStrings(entry, out);
		}
		return out;
	}
	if (value && typeof value === "object") {
		for (const entry of Object.values(value)) {
			collectStrings(entry, out);
		}
	}
	return out;
}

function nthIndexOf(text: string, needle: string, occurrence: number): number {
	let start = 0;
	for (let index = 1; index <= occurrence; index += 1) {
		const found = text.indexOf(needle, start);
		if (found < 0) {
			return -1;
		}
		if (index === occurrence) {
			return found;
		}
		start = found + needle.length;
	}
	return -1;
}

const grammarPath = join(root, "syntaxes/musi.tmLanguage.json");
const grammar = readJson<unknown>("syntaxes/musi.tmLanguage.json");
const codeblock = readJson<unknown>("syntaxes/musi_codeblock.tmLanguage.json");
const snippets = readJson<Record<string, Snippet>>(
	"snippets/musi_snippets.json",
);
const fixtures = readJson<GrammarFixtures>("scripts/grammar-fixtures.json");

const grammarText = collectStrings(grammar).join("\n");
const codeblockText = collectStrings(codeblock).join("\n");
const snippetPrefixes = Object.values(snippets).map(
	(snippet) => snippet.prefix,
);
const repositoryKeys =
	grammar && typeof grammar === "object" && "repository" in grammar
		? Object.keys(
				(grammar as { repository: Record<string, unknown> }).repository,
			)
		: [];
const grammarJson = JSON.stringify(grammar);

test("grammar uses canonical repository structure", () => {
	for (const key of fixtures.requiredRepositoryKeys ?? []) {
		expect(repositoryKeys).toContain(key);
	}

	for (const key of fixtures.forbiddenRepositoryKeys ?? []) {
		expect(repositoryKeys).not.toContain(key);
	}
});

test("grammar contains canonical keyword, operator, documentation, and scope patterns", () => {
	for (const keyword of fixtures.requiredKeywordPatterns) {
		expect(grammarText).toContain(keyword);
	}

	for (const operator of fixtures.requiredOperatorPatterns) {
		expect(grammarText).toContain(operator);
	}

	for (const pattern of fixtures.requiredDocumentationPatterns ?? []) {
		expect(grammarText).toContain(pattern);
	}

	for (const sample of fixtures.documentationCommentSamples ?? []) {
		expect(
			sample.includes("---") ||
				sample.includes("--!") ||
				sample.includes("/--") ||
				sample.includes("/-!"),
		).toBe(true);
		expect(sample).toContain("@");
		expect(
			sample.includes(":=") ||
				sample.includes("{@") ||
				sample.includes("http://") ||
				sample.includes("https://"),
		).toBe(true);
	}

	for (const scope of fixtures.requiredScopePatterns ?? []) {
		expect(grammarJson).toContain(scope);
	}

	for (const scope of fixtures.forbiddenScopePatterns ?? []) {
		expect(grammarJson).not.toContain(scope);
	}

	expect(grammarJson).toContain(fixtures.requiredAccessorScope);
	expect(grammarJson).not.toContain("\\\\.\\\\{|\\\\.\\\\[");
});

test("snippets expose canonical editor prefixes only", () => {
	for (const prefix of fixtures.requiredSnippetPrefixes) {
		expect(snippetPrefixes).toContain(prefix);
	}

	expect(snippetPrefixes).toHaveLength(fixtures.requiredSnippetPrefixes.length);
});

test("markdown grammar injects Musi code fences", () => {
	expect(codeblockText).toContain("musi|ms");
});

test("token fixtures expose expected TextMate scopes", async () => {
	const textmate = await import("vscode-textmate");
	const oniguruma = await import("vscode-oniguruma");
	const wasmPath = requireFromScript.resolve(
		"vscode-oniguruma/release/onig.wasm",
	);
	const wasm = readFileSync(wasmPath);
	const wasmBuffer = wasm.buffer.slice(
		wasm.byteOffset,
		wasm.byteOffset + wasm.byteLength,
	);
	await oniguruma.loadWASM(wasmBuffer);

	const grammarSource = readFileSync(grammarPath, "utf8");
	const registry = new textmate.Registry({
		onigLib: Promise.resolve({
			createOnigScanner(patterns: string[]) {
				return new oniguruma.OnigScanner(patterns);
			},
			createOnigString(text: string) {
				return new oniguruma.OnigString(text);
			},
		}),
		loadGrammar: async (scopeName: string) => {
			if (scopeName !== "source.musi") {
				return null;
			}
			return await textmate.parseRawGrammar(grammarSource, grammarPath);
		},
	});

	const tmGrammar = await registry.loadGrammar("source.musi");
	expect(tmGrammar).not.toBeNull();
	verifyTokenFixtures(tmGrammar, textmate.INITIAL);
});

function verifyTokenFixtures(
	tmGrammar: IGrammar | null,
	initialStack: StateStack,
): void {
	if (!tmGrammar) {
		throw new Error("failed to load TextMate grammar for token fixtures");
	}

	for (const fixture of fixtures.tokenFixtures ?? []) {
		const lines = fixture.source.split("\n");
		const tokenLines: IToken[][] = [];
		let stack = initialStack;

		for (const line of lines) {
			const tokenized = tmGrammar.tokenizeLine(line, stack);
			tokenLines.push(tokenized.tokens);
			stack = tokenized.ruleStack;
		}

		for (const check of fixture.checks ?? []) {
			verifyTokenCheck(fixture, lines, tokenLines, check);
		}
	}
}

function verifyTokenCheck(
	fixture: TokenFixture,
	lines: string[],
	tokenLines: IToken[][],
	check: TokenCheck,
): void {
	const line = lines[check.line] ?? "";
	const tokens = tokenLines[check.line] ?? [];
	const occurrence = check.occurrence ?? 1;
	const index = nthIndexOf(line, check.lexeme, occurrence);
	if (index < 0) {
		throw new Error(
			`${fixture.name}: lexeme \`${check.lexeme}\` occurrence ${occurrence} not found on line ${check.line}`,
		);
	}

	const token = tokens.find(
		(candidate) => candidate.startIndex <= index && index < candidate.endIndex,
	);
	if (!token) {
		throw new Error(
			`${fixture.name}: no token at line ${check.line} index ${index} for lexeme \`${check.lexeme}\``,
		);
	}

	if (!token.scopes.includes(check.scope)) {
		throw new Error(
			`${fixture.name}: lexeme \`${check.lexeme}\` missing scope \`${check.scope}\`; got [${token.scopes.join(", ")}]`,
		);
	}

	for (const forbiddenScope of check.forbiddenScopes ?? []) {
		if (token.scopes.includes(forbiddenScope)) {
			throw new Error(
				`${fixture.name}: lexeme \`${check.lexeme}\` unexpectedly had scope \`${forbiddenScope}\`; got [${token.scopes.join(", ")}]`,
			);
		}
	}
}
