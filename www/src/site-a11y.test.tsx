import { readFileSync } from "node:fs";
import { renderToString } from "preact-render-to-string";
import { describe, expect, it } from "vitest";
import { routeForPath } from "./routes";
import { App } from "./site-app";

const hiddenDisplayPattern =
	/\[hidden\]\s*\{[^}]*display:\s*none\s*!important;/;
const installCommandClipPattern =
	/\.install-command-group\s*\{[^}]*overflow:\s*clip/s;
const docsChildCuePinnedPattern =
	/\.docs-child-link__cue\s*\{[^}]*position:\s*absolute/s;
const rootLangPattern = /<html\s+lang=(?:\{route\.locale\}|"en")/;
const notFoundRoutePattern = /locale:\s*"en"/;

function render(path: string) {
	return renderToString(<App route={routeForPath(path)} />);
}

function countOccurrences(text: string, value: string) {
	return text.split(value).length - 1;
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
		expect(notFoundDocument).toMatch(notFoundRoutePattern);
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
		expect(html).toContain("Chapter tools");
		expect(html).toContain("On this page");
		expect(countOccurrences(html, "On this page")).toBe(1);
		expect(html).toContain("Chapter navigation");
	});

	it("renders deep docs pages with sidebar ancestry", () => {
		const html = render("/learn/guides/rust/mutation");
		expect(html).toContain("Musi for Developers");
		expect(html).toContain("Musi for Rust Developers");
		expect(html).toContain("Mutation");
		expect(html).toContain("Documentation sections");
	});

	it("renders descriptive docs section links on docs index", () => {
		const html = render("/learn");
		expect(html).toContain('class="mx-groupbox docs-groupbox"');
		expect(html).toContain(
			'class="mx-button mx-button--link docs-groupbox__open"',
		);
		expect(html).toContain("docs-child-card");
		expect(html).toContain("mx-action-card");
		expect(html).toContain("»");
		expect(html).not.toContain("docs-child-link");
		expect(html).not.toContain("&gt;&gt;");
		expect(html).not.toContain("Open section: Start");
	});

	it("renders icon utility controls with accessible labels", () => {
		const html = render("/");
		expect(html).toContain("GitHub repository");
		expect(html).not.toContain("Switch from system theme to light theme");
		expect(html).toContain('src="/favicon.svg"');
	});

	it("renders community links section", () => {
		const html = render("/community");
		expect(html).toContain("Community Links");
		expect(html).toContain('class="mx-groupbox community-link-group"');
		expect(html).toContain('class="community-link-box"');
		expect(html).toContain("Open GitHub");
		expect(html).toContain("Repository-first project");
		expect(html).toContain("Use public tracking");
		expect(html).not.toContain('legend="Notes"');
		expect(html).not.toContain("Community notes");
		expect(html).not.toContain('class="mx-action-card__title">Repository');
	});

	it("renders descriptive home card links", () => {
		const html = render("/");
		expect(html).toContain("Read Musi Book");
		expect(html).toContain("Open install path");
		expect(html).toContain("Open project links");
	});

	it("renders docs search on portal and docs pages", () => {
		for (const path of ["/", "/learn"]) {
			const html = render(path);
			expect(html).toContain('data-docs-search="true"');
			expect(html).toContain("Search docs");
		}
		const docsPage = render("/learn/book/start/getting-started");
		expect(docsPage).toContain('data-docs-search="true"');
		expect(docsPage).toContain("Search chapters");
		expect(docsPage).not.toContain(
			'<span class="mx-field__label">Search docs</span>',
		);
	});

	it("keeps portal docs search expanded and doc page docs search collapsed", () => {
		const home = render("/");
		const learn = render("/learn");
		expect(home).toContain("docs-search docs-search--panel");
		expect(countOccurrences(home, "Find docs")).toBe(1);
		expect(learn).toContain("docs-search docs-search--panel");
		expect(countOccurrences(learn, "Find docs")).toBe(1);
		const html = render("/learn/book/start/getting-started");
		expect(html).toContain("docs-search docs-search--disclosure");
		expect(html).toContain('data-docs-search-details="true"');
		expect(html).not.toContain('docs-search--disclosure" open');
		expect(html).not.toContain(
			'<div class="mx-section__title"><span>Find docs',
		);
		expect(html).not.toContain("site-toc-mobile");
		expect(html).not.toContain("site-toc-desktop");
		expect(html).toContain("docs-page-rail");
		expect(html).toContain("mx-sidebar--standalone");
		expect(html).toContain("mx-sidebar__link");
		expect(countOccurrences(html, "Jump to chapter")).toBe(1);
		expect(countOccurrences(html, "Search docs")).toBe(0);
	});

	it("keeps playground status cards concise", () => {
		const html = render("/playground");
		expect(html).toContain("Local install");
		expect(html).toContain("Use local runner");
		expect(html).toContain("Use source install for checks and tests.");
		expect(html).toContain("Read examples");
		expect(html).toContain("Open runnable examples and commands.");
		expect(html).toContain("Track work");
		expect(html).toContain(
			"Follow playground and runtime work in public issues.",
		);
		expect(html).not.toContain("Browser runner not public");
		expect(html).not.toContain(
			"Use the source install path for real checks and tests today.",
		);
		expect(html).not.toContain(
			"The book keeps examples close to supported syntax and commands.",
		);
		expect(html).not.toContain(
			"Issues and repository links show current playground and runtime work.",
		);
	});

	it("renders portal directory status and task paths", () => {
		const html = render("/");
		expect(html).toContain("Book-first");
		expect(html).toContain("Read Musi Book");
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
		expect(html).toContain('class="install-step-list"');
		expect(html).toContain("Check prerequisites");
		expect(html).toContain("Choose command lane");
		expect(html).toContain("Verify PATH");
		expect(html).toContain("Use current commands");
	});

	it("renders only one visible install script tab panel by default", () => {
		const html = render("/install");
		expect(html).toContain('data-code-tab-panel="curl"');
		expect(html).toContain('data-code-tab-panel="powershell" hidden');
	});

	it("does not nest raw Machines code frames in home or install snippets", () => {
		const home = render("/");
		const install = render("/install");
		const docsComparison = render("/learn/guides/c99/blocks-control-flow");
		expect(home).toContain("Start with one value, one function");
		const sampleFrameClass = 'class="mx-code-frame home-hero-sample"';
		expect(home.indexOf("home-sample-copy")).toBeGreaterThanOrEqual(0);
		expect(home.indexOf(sampleFrameClass)).toBeGreaterThanOrEqual(0);
		expect(home.indexOf("home-sample-copy")).toBeLessThan(
			home.indexOf(sampleFrameClass),
		);
		expect(home).not.toContain('class="mx-code-frame docs-code-sample"');
		expect(countOccurrences(home, sampleFrameClass)).toBe(1);
		expect(install).toContain('class="install-command-grid"');
		expect(install).toContain('class="mx-code-tabs"');
		expect(install).toContain('class="mx-code-frame__content"');
		expect(install).not.toContain('class="mx-code-frame"><section');
		expect(docsComparison).toContain('class="mx-code-tabs"');
		expect(docsComparison).toContain('data-code-tab-panel="source"');
		expect(docsComparison).toContain('data-code-tab-panel="musi" hidden');
	});

	it("keeps install command groupboxes and code tabs from clipping", () => {
		const siteCss = readFileSync(
			new URL("./styles/_content.scss", import.meta.url),
			"utf8",
		);
		const codeCss = readFileSync(
			new URL("./styles/_code.scss", import.meta.url),
			"utf8",
		);
		const machinesCss = readFileSync(
			new URL("../../packages/machines/src/components.scss", import.meta.url),
			"utf8",
		);

		expect(siteCss).not.toMatch(installCommandClipPattern);
		expect(machinesCss).toContain("overflow-x: auto;");
		expect(machinesCss).toContain("flex-wrap: nowrap;");
		expect(machinesCss).toContain("white-space: nowrap;");
		expect(machinesCss).toContain("overflow-y: hidden;");
		expect(machinesCss).toContain("margin-block: 0;");
		expect(codeCss).toContain(".docs-content>.mx-code-tabs");
		expect(codeCss).not.toContain(".docs-content .mx-code-frame,\n");
	});

	it("keeps install command groups stacked in one column", () => {
		const responsiveCss = readFileSync(
			new URL("./styles/_responsive.scss", import.meta.url),
			"utf8",
		);
		const contentCss = readFileSync(
			new URL("./styles/_content.scss", import.meta.url),
			"utf8",
		);

		expect(contentCss).toContain(".install-command-grid");
		expect(contentCss).toContain("grid-template-columns: minmax(0, 1fr);");
		expect(responsiveCss).not.toContain(
			".install-command-grid {\n\t\tgrid-template-columns: repeat(3",
		);
	});

	it("keeps wide desktop shells centered and docs cues inset", () => {
		const tokensCss = readFileSync(
			new URL("./styles/_tokens.scss", import.meta.url),
			"utf8",
		);
		const chromeCss = readFileSync(
			new URL("./styles/_chrome.scss", import.meta.url),
			"utf8",
		);
		const contentCss = readFileSync(
			new URL("./styles/_content.scss", import.meta.url),
			"utf8",
		);

		expect(tokensCss).toContain(
			"--site-shell-width: min(68rem, var(--mx-width));",
		);
		expect(chromeCss).toContain("var(--site-shell-width)");
		expect(contentCss).toContain(".site-heading .site-crumbs a");
		expect(contentCss).toContain("var(--mx-on-header)");
		expect(contentCss).not.toMatch(docsChildCuePinnedPattern);
	});

	it("keeps standalone sidebar panels outlined by the Machines theme", () => {
		const machinesCss = readFileSync(
			new URL("../../packages/machines/src/components.scss", import.meta.url),
			"utf8",
		);
		const chromeCss = readFileSync(
			new URL("./styles/_chrome.scss", import.meta.url),
			"utf8",
		);

		expect(machinesCss).toContain(".mx-sidebar--standalone");
		expect(machinesCss).toContain("border: 1px solid var(--mx-border-strong);");
		expect(machinesCss).toContain("box-shadow: var(--mx-shadow);");
		expect(chromeCss).not.toContain("site-toc-mobile");
		expect(chromeCss).not.toContain("site-toc-desktop");
	});

	it("keeps docs pages as a two-column reading hub", () => {
		const html = render("/learn/book/start/getting-started");
		const responsiveCss = readFileSync(
			new URL("./styles/_responsive.scss", import.meta.url),
			"utf8",
		);
		const contentCss = readFileSync(
			new URL("./styles/_content.scss", import.meta.url),
			"utf8",
		);

		expect(html).toContain("docs-body-grid");
		expect(html).toContain("docs-page-rail");
		expect(html).toContain("docs-article");
		expect(html.indexOf("docs-article")).toBeLessThan(
			html.indexOf("docs-page-rail"),
		);
		expect(html).not.toContain('class="docs-sidebar"');
		expect(responsiveCss).not.toContain(".site-shell-docs .site-frame");
		expect(responsiveCss).not.toContain(".docs-sidebar");
		expect(responsiveCss).toContain('grid-template-areas: "article rail";');
		expect(contentCss).toContain('"rail"');
		expect(contentCss).toContain('"article"');
	});

	it("keeps Machines and site chrome light-only", () => {
		const tokensCss = readFileSync(
			new URL("../../packages/machines/src/tokens.scss", import.meta.url),
			"utf8",
		);
		const componentsCss = readFileSync(
			new URL("../../packages/machines/src/components.scss", import.meta.url),
			"utf8",
		);
		const indexTs = readFileSync(
			new URL("../../packages/machines/src/index.ts", import.meta.url),
			"utf8",
		);
		const siteDocument = readFileSync(
			new URL("./layout/site-document.astro", import.meta.url),
			"utf8",
		);
		const staticScript = readFileSync(
			new URL("./static-main.inline.js", import.meta.url),
			"utf8",
		);

		for (const source of [
			tokensCss,
			componentsCss,
			indexTs,
			siteDocument,
			staticScript,
		]) {
			expect(source).not.toContain("midnight");
			expect(source).not.toContain("data-mx-scheme");
			expect(source).not.toContain("prefers-color-scheme");
			expect(source).not.toContain("musi-color-scheme");
			expect(source).not.toContain("theme-toggle");
		}
		expect(tokensCss).toContain("color-scheme: light;");
		expect(tokensCss).not.toContain("color-scheme: dark;");
		expect(indexTs).toContain(
			'export const themeNames = ["warm", "custom"] as const;',
		);
		expect(componentsCss).toContain("color: var(--mx-strip-text);");
		expect(componentsCss).toContain("color: var(--mx-strip-link);");
		expect(componentsCss).toContain("color: var(--mx-strip-accent);");
	});

	it("keeps strip table headers and docs child cards on warm tokens", () => {
		const codeCss = readFileSync(
			new URL("./styles/_code.scss", import.meta.url),
			"utf8",
		);
		const contentCss = readFileSync(
			new URL("./styles/_content.scss", import.meta.url),
			"utf8",
		);

		expect(codeCss).toContain(".mx-table-wrap thead th");
		expect(codeCss).toContain("background: var(--mx-gradient-subtitle);");
		expect(codeCss).toContain("color: var(--mx-strip-text);");
		expect(contentCss).toContain(".docs-child-card.mx-action-card");
		expect(contentCss).not.toContain(
			".docs-child-card.mx-action-card {\n\tmin-height: 0;\n\tbackground:",
		);
	});

	it("keeps site favicon unmasked in the header brand", () => {
		const css = readFileSync(
			new URL("./styles/_chrome.scss", import.meta.url),
			"utf8",
		);
		expect(css).toContain(".site-logo-mark.mx-brand__mark");
		expect(css).toContain("clip-path: none;");
		expect(css).toContain("background: transparent;");
	});

	it("keeps hidden tab panels hidden against component display rules", () => {
		const css = readFileSync(
			new URL("./styles/_base.scss", import.meta.url),
			"utf8",
		);
		expect(css).toMatch(hiddenDisplayPattern);
	});
});
