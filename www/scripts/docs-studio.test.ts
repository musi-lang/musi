import { join } from "node:path";
import { describe, expect, it } from "vitest";
import {
	docsStudioRoots,
	listDocsStudioFiles,
	normalizeDocsStudioPath,
	renderDocsStudioMarkdown,
} from "./docs-studio";

const repoRoot = join(import.meta.dirname, "..", "..");

describe("docs studio", () => {
	it("lists language, website, and landing Markdown files", async () => {
		const files = await listDocsStudioFiles();
		const paths = files.map((file) => file.path);

		expect(docsStudioRoots().map((root) => root.id)).toEqual([
			"language",
			"website",
			"landings",
		]);
		expect(paths).toContain("docs/what/language/index.md");
		expect(paths).toContain("docs/what/website/content-system.md");
		expect(paths).toContain("www/src/content/book/language/start/index.md");
	});

	it("accepts only configured docs roots", () => {
		expect(
			normalizeDocsStudioPath("docs/what/language/start/getting-started.md")
				.absolutePath,
		).toBe(join(repoRoot, "docs/what/language/start/getting-started.md"));

		expect(() =>
			normalizeDocsStudioPath("../musi-next/README.md"),
		).toThrowError("outside allowed roots");
		expect(() => normalizeDocsStudioPath("README.md")).toThrowError(
			"outside allowed roots",
		);
		expect(() =>
			normalizeDocsStudioPath("www/src/content/snippet-registry.ts"),
		).toThrowError("outside allowed roots");
	});

	it("renders local Markdown previews", () => {
		expect(renderDocsStudioMarkdown("# Title")).toContain("<h1>Title</h1>");
	});
});
