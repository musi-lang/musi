import { describe, expect, it } from "vitest";
import { appRoutes, normalizePath, routeForPath } from "./routes";

describe("routes", () => {
	it("normalizes trailing slashes", () => {
		expect(normalizePath("/install/")).toBe("/install");
	});

	it("falls back to english home for unknown paths", () => {
		expect(routeForPath("/missing").id).toBe("home:en");
	});

	it("resolves localized docs pages", () => {
		expect(routeForPath("/learn/language/effects-runtime/effects").kind).toBe(
			"doc",
		);
	});

	it("keeps learn landing separate from chapter slugs", () => {
		expect(routeForPath("/learn").kind).toBe("docs-index");
		expect(routeForPath("/learn/language/types/type-annotations").kind).toBe(
			"doc",
		);
	});

	it("does not expose blog routes", () => {
		expect(appRoutes.some((route) => route.path === "/blog")).toBe(false);
		expect(appRoutes.some((route) => route.path === "/ja/blog")).toBe(false);
	});
});
