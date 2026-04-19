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

	it("keeps language guide neighbors inside the selected guide", () => {
		expect(docNeighbors("musi-for-rust-developers").previous).toBeUndefined();
		expect(docNeighbors("musi-for-rust-developers").next?.id).toBe(
			"rust-values-functions",
		);
		expect(docNeighbors("rust-mutation").previous?.id).toBe(
			"rust-values-functions",
		);
		expect(docNeighbors("rust-mutation").next?.id).toBe("rust-records-structs");
		expect(docNeighbors("python-native-unsafe-ffi").previous?.id).toBe(
			"python-testing-tooling",
		);
		expect(docNeighbors("python-native-unsafe-ffi").next).toBeUndefined();
	});

	it("keeps non-guide chapter neighbors in book order", () => {
		const readingNeighbors = docNeighbors("reading-musi-code");
		expect(readingNeighbors.previous?.id).toBe("first-program");
		expect(readingNeighbors.next?.id).toBe("values-and-let");

		const neighbors = docNeighbors("values-and-let");
		expect(neighbors.previous?.id).toBe("reading-musi-code");
		expect(neighbors.next?.id).toBe("blocks-and-expressions");
	});

	it("finds docs by current route path", () => {
		expect(docForPath("/learn/book/types/foundations")).toBeUndefined();
		expect(docForPath("/learn/book/types/type-annotations")?.kind).toBe(
			"chapter",
		);
		expect(docForPath("/learn/book/types/type-annotations")?.title).toBe(
			"Type Annotations",
		);
		expect(docForPath("/learn/language/core/methods")?.title).toBe("Dot Calls");
		expect(docForPath("/learn/book/developers/rust")).toBeUndefined();
		expect(docForPath("/learn/guides/rust")?.kind).toBe("section");
		expect(docForPath("/learn/guides/rust/overview")?.kind).toBe("chapter");
		expect(docForPath("/learn/book/developers/rust/overview")).toBeUndefined();
		expect(docForPath("/learn/book/developers/rust/mutation")).toBeUndefined();
		expect(docForPath("/docs/book/developers/mutation")).toBeUndefined();
		expect(docForPath("/learn/guides/rust/mutation")?.title).toBe("Mutation");
		expect(docForPath("/docs/guides/rust/mutation")?.title).toBe("Mutation");
		expect(docForPath("/learn/guides/javascript-typescript")?.kind).toBe(
			"section",
		);
		expect(
			docForPath("/learn/guides/javascript-typescript/overview")?.kind,
		).toBe("chapter");
		expect(docForPath("/learn/guides/c99")?.kind).toBe("section");
		expect(docForPath("/learn/guides/c99/overview")?.kind).toBe("chapter");
		expect(docForPath("/learn/guides/cpp17")?.kind).toBe("section");
		expect(docForPath("/learn/guides/cpp17/overview")?.kind).toBe("chapter");
		expect(
			docForPath("/learn/book/developers/c/null-option-result"),
		).toBeUndefined();
		expect(docForPath("/learn/guides/c/null-option-result")?.title).toBe(
			"Null, Option, and Result",
		);
		expect(docForPath("/learn/guides/csharp")?.kind).toBe("section");
		expect(docForPath("/learn/guides/csharp/overview")?.kind).toBe("chapter");
		expect(
			docForPath("/learn/book/developers/csharp/null-option-result"),
		).toBeUndefined();
		expect(docForPath("/learn/guides/go")?.kind).toBe("section");
		expect(docForPath("/learn/guides/go/overview")?.kind).toBe("chapter");
		expect(
			docForPath("/learn/book/developers/go/nil-option-result"),
		).toBeUndefined();
		expect(docForPath("/learn/guides/java")?.kind).toBe("section");
		expect(docForPath("/learn/guides/java/overview")?.kind).toBe("chapter");
		expect(
			docForPath("/learn/book/developers/java/null-option-result"),
		).toBeUndefined();
		expect(docForPath("/learn/guides/lua")?.kind).toBe("section");
		expect(docForPath("/learn/guides/lua/overview")?.kind).toBe("chapter");
		expect(
			docForPath("/learn/book/developers/lua/nil-option-result"),
		).toBeUndefined();
		expect(docForPath("/learn/guides/python")?.kind).toBe("section");
		expect(docForPath("/learn/guides/python/overview")?.kind).toBe("chapter");
		expect(
			docForPath("/learn/book/developers/python/none-option-result"),
		).toBeUndefined();
		expect(docForPath("/learn/book/developers/javascript")).toBeUndefined();
		expect(docForPath("/learn/book/developers/typescript")).toBeUndefined();
	});

	it("builds breadcrumbs for deep developer pages", () => {
		expect(docBreadcrumb("rust-mutation").map((page) => page.slug)).toEqual([
			"developers",
			"rust",
			"mutation",
		]);
	});

	it("keeps adjusted guides visible in alphabetical order under language guides", () => {
		const titles = docChildren("developers").map((page) => page.title);
		expect(titles).toEqual([
			"Musi for C Developers",
			"Musi for C++ Developers",
			"Musi for C# Developers",
			"Musi for Go Developers",
			"Musi for Java Developers",
			"Musi for JavaScript and TypeScript Developers",
			"Musi for Lua Developers",
			"Musi for Python Developers",
			"Musi for Rust Developers",
		]);
	});

	it("groups docs for the sidebar and index", () => {
		expect(docGroups[0]?.group).toBe("Start");
		expect(
			docGroups.some((group) => group.group === "Advanced and Tooling"),
		).toBe(true);
		expect(
			docGroups.some((group) => group.group === "Musi for Developers"),
		).toBe(false);
	});

	it("extracts headings for page toc", () => {
		expect(
			docForPath("/learn/book/effects-runtime/effects")?.headings.length,
		).toBeGreaterThan(0);
	});
});
