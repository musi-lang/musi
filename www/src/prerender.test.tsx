import { describe, expect, it } from "vitest";
import { renderRoute } from "./prerender";
import { appRoutes } from "./routes";

describe("prerender", () => {
	it("renders each public route", () => {
		for (const route of appRoutes) {
			const html = renderRoute(route);
			expect(html).toContain("Musi");
		}
	});
});
