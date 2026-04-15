import { describe, expect, it } from "vitest";
import { docForPath, docGroups, docNeighbors, docsPages } from "./docs";

describe("docs", () => {
	it("loads english book chapters", () => {
		expect(docsPages[0]?.id).toBe("getting-started");
		expect(docsPages.length).toBe(45);
	});

	it("returns neighbors for middle pages", () => {
		const neighbors = docNeighbors("packages");
		expect(neighbors.previous?.slug).toBe("files");
		expect(neighbors.next?.slug).toBe("imports-and-exports");
	});

	it("finds docs by current route path", () => {
		expect(docForPath("/learn/language/types/type-annotations")?.title).toBe(
			"Type annotations",
		);
		expect(docForPath("/learn/language/core/methods")?.title).toBe("Methods");
	});

	it("groups docs for the sidebar and index", () => {
		expect(docGroups[0]?.group).toBe("Start");
		expect(docGroups.at(-1)?.group).toBe("Advanced and tooling");
	});

	it("extracts headings for page toc", () => {
		expect(
			docForPath("/learn/language/effects-runtime/effects")?.headings.length,
		).toBeGreaterThan(0);
	});
});
