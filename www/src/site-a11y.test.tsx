import { readFileSync } from "node:fs";
import { renderToString } from "preact-render-to-string";
import { describe, expect, it } from "vitest";
import { routeForPath } from "./routes";
import { App } from "./site-app";

const hiddenDisplayPattern =
	/\[hidden\]\s*\{[^}]*display:\s*none\s*!important;/;
const rootLangPattern = /<html\s+lang=(?:\{route\.locale\}|"en")/;

function render(path: string) {
	return renderToString(<App route={routeForPath(path)} />);
}

describe("site accessibility scaffolding", () => {
	it("keeps document shells rooted with English lang metadata", () => {
		const siteDocument = readFileSync(
			new URL("./layout/site-document.astro", import.meta.url),
			"utf8",
		);
		const notFoundDocument = readFileSync(
			new URL("./pages/404.astro", import.meta.url),
			"utf8",
		);

		expect(siteDocument).toMatch(rootLangPattern);
		expect(notFoundDocument).toMatch(rootLangPattern);
	});

	it("does not render nested html nodes inside the app body", () => {
		for (const path of ["/", "/learn", "/install"]) {
			const html = render(path);
			expect(html).not.toContain("<html");
		}
	});

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
		const html = render("/learn/book/start/getting-started");
		expect(html).toContain("Documentation sections");
		expect(html).toContain("On this page");
		expect(html).toContain("Chapter Navigation");
	});

	it("renders deep docs pages with sidebar ancestry", () => {
		const html = render("/learn/book/developers/guides/rust/mutation");
		expect(html).toContain("Language Guides");
		expect(html).toContain("Musi for Rust Developers");
		expect(html).toContain("Mutation");
		expect(html).toContain("Documentation sections");
	});

	it("renders descriptive docs section links on docs index", () => {
		const html = render("/learn");
		expect(html).toContain("Open section: Start");
	});

	it("renders icon utility controls with accessible labels", () => {
		const html = render("/");
		expect(html).toContain("GitHub repository");
		expect(html).toContain("Switch from system theme to light theme");
	});

	it("renders community links section", () => {
		const html = render("/community");
		expect(html).toContain("Community Links");
		expect(html).toContain("Open GitHub");
	});

	it("renders descriptive home card links", () => {
		const html = render("/");
		expect(html).toContain("Read Musi Book");
		expect(html).toContain("Open install guide");
		expect(html).toContain("Open community links");
	});

	it("renders docs search on portal and docs pages", () => {
		for (const path of ["/", "/learn", "/learn/book/start/getting-started"]) {
			const html = render(path);
			expect(html).toContain('data-docs-search="true"');
			expect(html).toContain("Search docs");
		}
	});

	it("keeps portal docs search expanded and doc page docs search collapsed", () => {
		expect(render("/")).toContain("docs-search docs-search-panel");
		expect(render("/learn")).toContain("docs-search docs-search-panel");
		const html = render("/learn/book/start/getting-started");
		expect(html).toContain("docs-search docs-search-disclosure");
		expect(html).toContain('data-docs-search-details="true"');
		expect(html).not.toContain('docs-search-disclosure" open');
	});

	it("renders portal directory status and task paths", () => {
		const html = render("/");
		expect(html).toContain("Book-first");
		expect(html).toContain("First file path");
		expect(html).toContain("Unusual cases");
	});

	it("renders install command table headers", () => {
		const html = render("/install");
		expect(html).toContain('<th scope="col">Lane</th>');
		expect(html).toContain('<th scope="col">Command</th>');
		expect(html).toContain('<th scope="col">Description</th>');
	});

	it("renders install process steps", () => {
		const html = render("/install");
		expect(html).toContain("Check prerequisites");
		expect(html).toContain("Choose command lane");
		expect(html).toContain("Verify PATH");
	});

	it("renders only one visible install script tab panel by default", () => {
		const html = render("/install");
		expect(html).toContain('data-code-tab-panel="curl"');
		expect(html).toContain('data-code-tab-panel="powershell" hidden');
	});

	it("keeps hidden tab panels hidden against component display rules", () => {
		const css = readFileSync(
			new URL("./styles/_base.scss", import.meta.url),
			"utf8",
		);
		expect(css).toMatch(hiddenDisplayPattern);
	});
});
