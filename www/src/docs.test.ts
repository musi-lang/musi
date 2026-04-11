import { describe, expect, it } from "vitest";
import { docForPath, docGroups, docNeighbors, docsPages } from "./docs";

describe("docs", () => {
	it("loads guided docs in order", () => {
		expect(docsPages[0]?.slug).toBe("getting-started");
		expect(docsPages.at(-1)?.slug).toBe("testing-and-running");
		expect(docsPages).toHaveLength(15);
	});

	it("returns neighbors for middle pages", () => {
		const neighbors = docNeighbors("imports-and-packages");
		expect(neighbors.previous?.slug).toBe("functions-and-calls");
		expect(neighbors.next?.slug).toBe("data-and-pattern-matching");
	});

	it("finds docs by route path", () => {
		expect(docForPath("/docs/types")?.title).toBe("Types and generics");
	});

	it("groups docs for the sidebar and index", () => {
		expect(docGroups[0]?.group).toBe("Start");
		expect(docGroups.at(-1)?.group).toBe("Tooling");
	});

	it("extracts headings for page toc", () => {
		expect(
			docForPath("/docs/effects-and-handlers")?.headings.length,
		).toBeGreaterThan(0);
	});
});
