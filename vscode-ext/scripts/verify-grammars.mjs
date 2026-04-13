// @ts-nocheck

import fs from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";
import { fileURLToPath } from "node:url";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const require = createRequire(import.meta.url);

function readJson(relativePath) {
	return JSON.parse(fs.readFileSync(path.join(root, relativePath), "utf8"));
}

function collectStrings(value, out = []) {
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

function assert(condition, message) {
	if (!condition) {
		throw new Error(message);
	}
}

function nthIndexOf(text, needle, occurrence) {
	let start = 0;
	for (let i = 1; i <= occurrence; i += 1) {
		const index = text.indexOf(needle, start);
		if (index < 0) {
			return -1;
		}
		if (i === occurrence) {
			return index;
		}
		start = index + needle.length;
	}
	return -1;
}

const grammarPath = path.join(root, "syntaxes/musi.tmLanguage.json");
const grammar = readJson("syntaxes/musi.tmLanguage.json");
const codeblock = readJson("syntaxes/musi_codeblock.tmLanguage.json");
const snippets = readJson("snippets/musi_snippets.json");
const fixtures = readJson("scripts/grammar-fixtures.json");

const grammarText = collectStrings(grammar).join("\n");
const codeblockText = collectStrings(codeblock).join("\n");
const snippetPrefixes = Object.values(snippets).map(
	(snippet) => snippet.prefix,
);
const grammarJson = JSON.stringify(grammar);

for (const keyword of fixtures.requiredKeywordPatterns) {
	assert(
		grammarText.includes(keyword),
		`grammar missing canonical keyword pattern \`${keyword}\``,
	);
}

for (const operator of fixtures.requiredOperatorPatterns) {
	assert(
		grammarText.includes(operator),
		`grammar missing canonical operator pattern \`${operator}\``,
	);
}

for (const pattern of fixtures.requiredDocumentationPatterns ?? []) {
	assert(
		grammarText.includes(pattern),
		`grammar missing documentation pattern \`${pattern}\``,
	);
}

for (const sample of fixtures.documentationCommentSamples ?? []) {
	assert(
		sample.includes("///") || sample.includes("/**"),
		`documentation sample missing doc comment opener: ${sample}`,
	);
	assert(
		sample.includes("@"),
		`documentation sample missing documentation tag: ${sample}`,
	);
	assert(
		sample.includes(":=") ||
			sample.includes("{@") ||
			sample.includes("http://") ||
			sample.includes("https://"),
		`documentation sample missing Musi documentation content: ${sample}`,
	);
}

for (const scope of fixtures.requiredScopePatterns ?? []) {
	assert(
		grammarJson.includes(scope),
		`grammar missing semantic-ready scope \`${scope}\``,
	);
}

for (const scope of fixtures.forbiddenScopePatterns ?? []) {
	assert(
		!grammarJson.includes(scope),
		`grammar still contains forbidden weak scope root \`${scope}\``,
	);
}

assert(
	grammarJson.includes(fixtures.requiredAccessorScope),
	`grammar missing accessor scope \`${fixtures.requiredAccessorScope}\``,
);
assert(
	!grammarJson.includes("\\\\.\\\\{|\\\\.\\\\["),
	"grammar still treats `.[` or `.{` as combined opener token",
);

for (const prefix of fixtures.requiredSnippetPrefixes) {
	assert(
		snippetPrefixes.includes(prefix),
		`snippets missing canonical prefix \`${prefix}\``,
	);
}

assert(
	snippetPrefixes.length === fixtures.requiredSnippetPrefixes.length,
	"snippets define prefixes outside canonical editor set",
);

assert(
	codeblockText.includes("musi|ms"),
	"markdown injection missing fenced `musi` or `ms` support",
);

const textmateModule = await import("vscode-textmate");
const onigurumaModule = await import("vscode-oniguruma");
const textmate = textmateModule.default ?? textmateModule;
const oniguruma = onigurumaModule.default ?? onigurumaModule;

const wasmPath = require.resolve("vscode-oniguruma/release/onig.wasm");
const wasm = fs.readFileSync(wasmPath);
const wasmBuffer = wasm.buffer.slice(
	wasm.byteOffset,
	wasm.byteOffset + wasm.byteLength,
);
await oniguruma.loadWASM(wasmBuffer);

const grammarSource = fs.readFileSync(grammarPath, "utf8");
const registry = new textmate.Registry({
	onigLib: Promise.resolve({
		createOnigScanner(patterns) {
			return new oniguruma.OnigScanner(patterns);
		},
		createOnigString(text) {
			return new oniguruma.OnigString(text);
		},
	}),
	loadGrammar: async (scopeName) => {
		if (scopeName !== "source.musi") {
			return null;
		}
		return await textmate.parseRawGrammar(grammarSource, grammarPath);
	},
});

const tmGrammar = await registry.loadGrammar("source.musi");
assert(tmGrammar, "failed to load TextMate grammar for token fixtures");

for (const fixture of fixtures.tokenFixtures ?? []) {
	const lines = fixture.source.split("\n");
	const tokenLines = [];
	let stack = textmate.INITIAL;

	for (const line of lines) {
		const tokenized = tmGrammar.tokenizeLine(line, stack);
		tokenLines.push(tokenized.tokens);
		stack = tokenized.ruleStack;
	}

	for (const check of fixture.checks ?? []) {
		const line = lines[check.line] ?? "";
		const tokens = tokenLines[check.line] ?? [];
		const occurrence = check.occurrence ?? 1;
		const index = nthIndexOf(line, check.lexeme, occurrence);
		assert(
			index >= 0,
			`${fixture.name}: lexeme \`${check.lexeme}\` occurrence ${occurrence} not found on line ${check.line}`,
		);
		const token = tokens.find(
			(candidate) =>
				candidate.startIndex <= index && index < candidate.endIndex,
		);
		assert(
			token,
			`${fixture.name}: no token at line ${check.line} index ${index} for lexeme \`${check.lexeme}\``,
		);
		assert(
			token.scopes.includes(check.scope),
			`${fixture.name}: lexeme \`${check.lexeme}\` missing scope \`${check.scope}\`; got [${token.scopes.join(", ")}]`,
		);
	}
}

console.log("grammar verification passed");
