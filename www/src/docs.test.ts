import { describe, expect, it } from "vitest";
import { docForPath, docGroups, docNeighbors, docsPages } from "./docs";

describe("docs", () => {
	it("loads book chapters in order", () => {
		expect(docsPages[0]?.id).toBe("getting-started");
		expect(docsPages.at(-1)?.id).toBe("common-questions");
		expect(docsPages.length).toBeGreaterThan(16);
	});

	it("returns neighbors for middle pages", () => {
		const neighbors = docNeighbors("imports-and-packages");
		expect(neighbors.previous?.slug).toBe("files-packages-and-entry");
		expect(neighbors.next?.slug).toBe("expressions-and-bindings");
	});

	it("finds docs by route path", () => {
		expect(docForPath("/docs/types-and-abstractions/types")?.title).toBe(
			"Types and generics",
		);
		expect(docForPath("/docs/operators-and-literals")?.title).toBe(
			"Operators and literals",
		);
	});

	it("groups docs for the sidebar and index", () => {
		expect(docGroups[0]?.group).toBe("Start");
		expect(docGroups.at(-1)?.group).toBe("Common questions");
	});

	it("extracts headings for page toc", () => {
		expect(
			docForPath("/docs/effects-and-handlers")?.headings.length,
		).toBeGreaterThan(0);
	});
});
