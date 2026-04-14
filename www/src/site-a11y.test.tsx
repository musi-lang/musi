import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";
import { App } from "./pages";
import { routeForPath } from "./routes";

function render(path: string) {
	return renderToStaticMarkup(<App route={routeForPath(path)} />);
}

describe("site accessibility scaffolding", () => {
	it("renders skip link and main landmark on key pages", () => {
		for (const path of [
			"/",
			"/learn",
			"/install",
			"/community",
			"/playground",
		]) {
			const html = render(path);
			expect(html).toContain('href="#main-content"');
			expect(html).toContain("<main");
			expect(html).toContain("Skip to main content");
		}
	});

	it("renders docs page with complementary navigation landmarks", () => {
		const html = render("/learn/language/start/getting-started");
		expect(html).toContain("Documentation sections");
		expect(html).toContain("On this page");
		expect(html).toContain("Chapter navigation");
	});

	it("renders descriptive docs section links on docs index", () => {
		const html = render("/learn");
		expect(html).toContain("Open section: Start");
	});

	it("renders icon utility controls with accessible labels", () => {
		const html = render("/");
		expect(html).toContain("GitHub repository");
		expect(html).toContain("Switch from system theme to light theme");
		expect(html).toContain("Switch language");
	});

	it("renders community links section", () => {
		const html = render("/community");
		expect(html).toContain("Community links");
		expect(html).toContain("Open GitHub");
	});

	it("renders descriptive home card links", () => {
		const html = render("/");
		expect(html).toContain("Read Learn docs");
		expect(html).toContain("Open install guide");
		expect(html).toContain("Open community links");
	});

	it("renders install command table headers", () => {
		const html = render("/install");
		expect(html).toContain('<th scope="col">Lane</th>');
		expect(html).toContain('<th scope="col">Command</th>');
		expect(html).toContain('<th scope="col">Description</th>');
	});
});
