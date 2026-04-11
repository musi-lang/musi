import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");

/**
 * @param {string} relativePath
 */
function readJson(relativePath) {
	return JSON.parse(fs.readFileSync(path.join(root, relativePath), "utf8"));
}

/**
 * @param {unknown} value
 * @param {string[]} [out]
 * @returns {string[]}
 */
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

/**
 * @param {unknown} condition
 * @param {string} message
 */
function assert(condition, message) {
	if (!condition) {
		throw new Error(message);
	}
}

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

assert(
	grammarJson.includes(fixtures.requiredAccessorScope),
	`grammar missing accessor scope \`${fixtures.requiredAccessorScope}\``,
);
assert(
	!grammarJson.includes("\\\\.\\\\{|\\\\.\\\\["),
	"grammar still treats `.[` or `.{` as a combined opener token",
);

for (const prefix of fixtures.requiredSnippetPrefixes) {
	assert(
		snippetPrefixes.includes(prefix),
		`snippets missing canonical prefix \`${prefix}\``,
	);
}

assert(
	snippetPrefixes.length === fixtures.requiredSnippetPrefixes.length,
	"snippets define prefixes outside the canonical editor set",
);

assert(
	codeblockText.includes("musi|ms"),
	"markdown injection missing fenced `musi` or `ms` support",
);

console.log("grammar verification passed");
