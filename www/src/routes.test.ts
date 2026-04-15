import { describe, expect, it } from "vitest";
import { appRoutes, homeRoutes, normalizePath, routeForPath } from "./routes";

describe("routes", () => {
	it("normalizes trailing slashes", () => {
		expect(normalizePath("/install/")).toBe("/install");
	});

	it("falls back to english home for unknown paths", () => {
		expect(routeForPath("/missing").id).toBe("home:en");
	});

	it("resolves docs pages", () => {
		expect(routeForPath("/learn/book/effects-runtime/effects").kind).toBe(
			"doc",
		);
	});

	it("keeps learn landing separate from chapter slugs", () => {
		expect(routeForPath("/learn").kind).toBe("docs-index");
		expect(routeForPath("/learn/book").kind).toBe("docs-index");
		expect(
			routeForPath("/learn/book/types/foundations/type-annotations").kind,
		).toBe("doc");
		expect(routeForPath("/learn/book/types/type-annotations").kind).toBe("doc");
		expect(routeForPath("/learn/language/types/type-annotations").kind).toBe(
			"doc",
		);
		expect(routeForPath("/learn/book/developers/guides/rust").kind).toBe("doc");
		expect(
			routeForPath("/learn/book/developers/guides/rust/mutation").kind,
		).toBe("doc");
		expect(routeForPath("/learn/book/developers/rust").kind).toBe("doc");
		expect(routeForPath("/learn/book/developers/rust/mutation").kind).toBe(
			"doc",
		);
		expect(routeForPath("/learn/language/developers/rust/mutation").kind).toBe(
			"doc",
		);
		expect(routeForPath("/docs/book/start/getting-started").kind).toBe("doc");
		expect(routeForPath("/docs/book/developers/mutation").kind).toBe("doc");
	});

	it("canonicalizes docs aliases to nested learn routes", () => {
		expect(routeForPath("/docs/book/start/getting-started").canonicalPath).toBe(
			"/learn/book/start/foundations/getting-started",
		);
		expect(routeForPath("/learn/book/developers/rust").canonicalPath).toBe(
			"/learn/book/developers/guides/rust",
		);
		expect(
			routeForPath("/learn/book/developers/rust/mutation").canonicalPath,
		).toBe("/learn/book/developers/guides/rust/mutation");
		expect(routeForPath("/learn").canonicalPath).toBe("/learn/book");
	});

	it("uses full browser title", () => {
		expect(homeRoutes[0]?.title).toBe("The Musi Programming Language");
		expect(routeForPath("/learn/book").title).toBe(
			"Musi Book | The Musi Programming Language",
		);
		expect(routeForPath("/learn/book/start/getting-started").title).toBe(
			"Getting Started | The Musi Programming Language",
		);
	});

	it("does not expose blog or ja routes", () => {
		expect(appRoutes.some((route) => route.path === "/blog")).toBe(false);
		expect(appRoutes.some((route) => route.path.startsWith("/ja"))).toBe(false);
	});
});
