import assert from "node:assert/strict";
import test from "node:test";

import {
	formatArgs,
	formatKindForDocument,
	shouldUseCliFormatter,
} from "./formatter-core.ts";

test("maps Musi documents to stdin formatter args", () => {
	assert.equal(formatKindForDocument("musi", "/workspace/index.ms"), "ms");
	assert.deepEqual(formatArgs("ms"), ["fmt", "--ext", "ms", "-"]);
});

test("maps editor indentation options to formatter args", () => {
	assert.deepEqual(formatArgs("ms", { insertSpaces: true, tabSize: 4 }), [
		"fmt",
		"--ext",
		"ms",
		"--indent-width",
		"4",
		"--use-spaces",
		"-",
	]);
	assert.deepEqual(formatArgs("ms", { insertSpaces: false, tabSize: 3 }), [
		"fmt",
		"--ext",
		"ms",
		"--indent-width",
		"3",
		"--use-tabs",
		"-",
	]);
});

test("maps Markdown documents to markdown formatter args", () => {
	assert.equal(
		formatKindForDocument("markdown", "/workspace/README.md"),
		"markdown",
	);
	assert.deepEqual(formatArgs("markdown"), ["fmt", "--ext", "markdown", "-"]);
});

test("skips Musi CLI provider while LSP is running unless explicit command", () => {
	assert.equal(shouldUseCliFormatter("ms", true, false), false);
	assert.equal(shouldUseCliFormatter("ms", true, true), true);
});

test("keeps Markdown CLI provider active while LSP is running", () => {
	assert.equal(shouldUseCliFormatter("markdown", true, false), true);
});
