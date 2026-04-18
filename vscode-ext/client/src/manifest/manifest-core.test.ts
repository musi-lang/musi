import assert from "node:assert/strict";
import test from "node:test";

import { packageEntry } from "./manifest-core.ts";

test("uses entry field for package entry", () => {
	assert.equal(packageEntry({ entry: "src/app.ms" }), "src/app.ms");
});

test("does not use legacy main field for package entry", () => {
	assert.equal(packageEntry({ main: "legacy.ms" }), "index.ms");
});
