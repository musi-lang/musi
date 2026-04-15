import { describe, expect, it } from "vitest";
import {
	docBreadcrumb,
	docChildren,
	docForPath,
	docGroups,
	docNeighbors,
	docsPages,
} from "./docs";

describe("docs", () => {
	it("loads english book chapters", () => {
		expect(docsPages[0]?.id).toBe("getting-started");
		expect(docsPages.length).toBeGreaterThanOrEqual(55);
	});

	it("returns neighbors through tree order", () => {
		const neighbors = docNeighbors("rust-mutation");
		expect(neighbors.previous?.slug).toBe("values-functions");
		expect(neighbors.next?.slug).toBe("records-structs");
	});

	it("finds docs by current route path", () => {
		expect(docForPath("/learn/book/types/foundations")?.kind).toBe("section");
		expect(
			docForPath("/learn/book/types/foundations/type-annotations")?.kind,
		).toBe("chapter");
		expect(docForPath("/learn/book/types/type-annotations")?.title).toBe(
			"Type Annotations",
		);
		expect(docForPath("/learn/language/core/methods")?.title).toBe("Methods");
		expect(docForPath("/learn/book/developers/rust")?.kind).toBe("section");
		expect(docForPath("/learn/book/developers/guides/rust")?.kind).toBe(
			"section",
		);
		expect(
			docForPath("/learn/book/developers/guides/rust/overview")?.kind,
		).toBe("chapter");
		expect(docForPath("/learn/book/developers/rust/overview")?.kind).toBe(
			"chapter",
		);
		expect(docForPath("/learn/book/developers/rust/mutation")?.title).toBe(
			"Mutation",
		);
		expect(
			docForPath("/learn/book/developers/guides/rust/mutation")?.title,
		).toBe("Mutation");
	});

	it("builds breadcrumbs for deep developer pages", () => {
		expect(docBreadcrumb("rust-mutation").map((page) => page.slug)).toEqual([
			"developers",
			"guides",
			"rust",
			"mutation",
		]);
	});

	it("keeps non-rust guides visible in alphabetical order under language guides", () => {
		const titles = docChildren("developers-guides").map((page) => page.title);
		expect(titles).toEqual([
			"Musi for C Developers",
			"Musi for C# Developers",
			"Musi for C++ Developers",
			"Musi for Go Developers",
			"Musi for Java Developers",
			"Musi for JavaScript Developers",
			"Musi for Python Developers",
			"Musi for Rust Developers",
			"Musi for TypeScript Developers",
		]);
	});

	it("groups docs for the sidebar and index", () => {
		expect(docGroups[0]?.group).toBe("Start");
		expect(
			docGroups.some((group) => group.group === "Advanced and Tooling"),
		).toBe(true);
		expect(
			docGroups.some((group) => group.group === "Musi for Developers"),
		).toBe(true);
	});

	it("extracts headings for page toc", () => {
		expect(
			docForPath("/learn/book/effects-runtime/effects")?.headings.length,
		).toBeGreaterThan(0);
	});
});
