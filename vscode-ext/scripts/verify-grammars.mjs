import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(__dirname, "..");

function readJson(relativePath) {
	const filePath = path.join(root, relativePath);
	return JSON.parse(fs.readFileSync(filePath, "utf8"));
}

const grammar = readJson("syntaxes/musi.tmLanguage.json");
const codeblock = readJson("syntaxes/musi_codeblock.tmLanguage.json");
const pkg = readJson("package.json");
const grammarText = JSON.stringify(grammar);
const keywordPatterns = grammar.repository.keywords?.patterns ?? [];
const operatorPatterns = grammar.repository.operators?.patterns ?? [];

assert.equal(grammar.scopeName, "source.musi");
assert.ok(grammar.repository, "grammar repository missing");

for (const key of [
	"attributes",
	"data-body",
	"effect-body",
	"class-body",
	"let-data-definition",
	"type-annotation",
	"where-clause",
	"effect-set",
]) {
	assert.ok(grammar.repository[key], `missing repository entry: ${key}`);
}

assert.ok(
	!grammarText.includes("Int|Float|Bool|String|Array|Record|Effect"),
	"builtin-name allowlist remains in the grammar",
);
assert.ok(
	!grammarText.includes('"name":"keyword.control.musi","match":"\\\\b(and|as|case|class|data|effect|export|foreign|handle|if|import|in|instance|law|let|mut|not|of|opaque|or|perform|quote|rec|resume|shl|shr|where|with|xor)\\\\b"'),
	"broad keyword.control catch-all remains in the grammar",
);

assert.ok(
	grammarText.includes("entity.name.type.musi"),
	"type scopes missing from grammar",
);
assert.ok(
	keywordPatterns.some((rule) => rule.name === "storage.type.declaration.musi"),
	"declaration keyword scope missing from grammar",
);
assert.ok(
	keywordPatterns.some(
		(rule) => rule.name === "keyword.control.conditional.musi",
	),
	"conditional keyword scope missing from grammar",
);
assert.ok(
	keywordPatterns.some((rule) => rule.name === "keyword.control.effect.musi"),
	"effect keyword scope missing from grammar",
);
assert.ok(
	keywordPatterns.some((rule) => rule.name === "keyword.control.import.musi"),
	"import keyword scope missing from grammar",
);
assert.ok(
	keywordPatterns.some((rule) => rule.name === "keyword.control.export.musi"),
	"export keyword scope missing from grammar",
);
assert.ok(
	keywordPatterns.some((rule) => rule.name === "keyword.other.constraint.musi"),
	"constraint keyword scope missing from grammar",
);
assert.ok(
	keywordPatterns.some((rule) => rule.name === "keyword.other.effect.musi"),
	"effect-set keyword scope missing from grammar",
);
assert.ok(
	keywordPatterns.some(
		(rule) => rule.name === "keyword.other.metaprogramming.musi",
	),
	"metaprogramming keyword scope missing from grammar",
);
assert.ok(
	operatorPatterns.some((rule) => rule.name === "keyword.operator.word.musi"),
	"word-operator scope missing from grammar",
);
assert.equal(codeblock.scopeName, "text.musi.codeblock");
assert.ok(
	JSON.stringify(codeblock).includes("ms|musi"),
	"markdown injection should recognize both ms and musi fences",
);

const semanticScopes = pkg.contributes.semanticTokenScopes?.[0]?.scopes;
assert.ok(semanticScopes, "semantic token scopes missing");
assert.deepEqual(semanticScopes.type, ["entity.name.type.musi"]);
assert.deepEqual(semanticScopes.function, ["entity.name.function.musi"]);

console.log("Musi VS Code grammars verified.");
