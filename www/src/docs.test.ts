import { describe, expect, it } from "vitest";
import { docForPath, docGroups, docNeighbors, docsPages } from "./docs";

describe("docs", () => {
	it("loads book chapters for both locales", () => {
		expect(docsPages[0]?.id).toBe("getting-started");
		expect(docsPages.length).toBeGreaterThan(30);
	});

	it("returns neighbors for middle pages per locale", () => {
		const neighbors = docNeighbors("imports-and-packages", "en");
		expect(neighbors.previous?.slug).toBe("files-packages-and-entry");
		expect(neighbors.next?.slug).toBe("expressions-and-bindings");
	});

	it("finds docs by localized route path", () => {
		expect(docForPath("/learn/types-and-abstractions/types")?.title).toBe(
			"Types and generics",
		);
		expect(
			docForPath("/ja/learn/core-language/operators-and-literals")?.title,
		).toBe("演算子とリテラル");
	});

	it("groups docs for the sidebar and index", () => {
		expect(docGroups.filter((group) => group.locale === "en")[0]?.group).toBe(
			"Start",
		);
		expect(
			docGroups.filter((group) => group.locale === "ja").at(-1)?.group,
		).toBe("よくある質問");
	});

	it("extracts headings for page toc", () => {
		expect(
			docForPath("/learn/types-and-abstractions/effects-and-handlers")?.headings
				.length,
		).toBeGreaterThan(0);
	});
});
