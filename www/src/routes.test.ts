import { describe, expect, it } from "vitest";
import { normalizePath, routeForPath } from "./routes";

describe("routes", () => {
	it("normalizes trailing slashes", () => {
		expect(normalizePath("/install/")).toBe("/install");
	});

	it("falls back to home for unknown paths", () => {
		expect(routeForPath("/missing").id).toBe("home");
	});

	it("resolves docs pages", () => {
		expect(routeForPath("/docs/effects-and-handlers").kind).toBe("doc");
	});

	it("keeps docs index separate from doc slugs", () => {
		expect(routeForPath("/docs").kind).toBe("docs-index");
		expect(routeForPath("/docs/types").kind).toBe("doc");
	});
});
