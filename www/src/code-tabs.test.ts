// @vitest-environment jsdom

import { beforeEach, describe, expect, it } from "vitest";
import { initCodeTabs, initCodeTabsWithin } from "./code-tabs";
import {
	comparisonLanguageLabels,
	comparisonLanguages,
} from "./content/examples/languages";

interface TestTabsDataset extends DOMStringMap {
	activeLanguage?: string;
	upgraded?: string;
}

function makeTabsMarkup() {
	const tabs = comparisonLanguages
		.map((language) => {
			const active = language === "musi";
			const label = comparisonLanguageLabels[language];
			return `<button type="button" role="tab" data-language="${language}" aria-selected="${active ? "true" : "false"}" tabindex="${active ? "0" : "-1"}">${label}</button>`;
		})
		.join("\n");

	const panels = comparisonLanguages
		.map((language) => {
			const hidden = language === "musi" ? "" : ' hidden=""';
			return `<section role="tabpanel" data-language="${language}"${hidden}></section>`;
		})
		.join("\n");

	return `
	<div data-code-tabs="1" data-example-id="demo" data-default="musi" data-active-language="musi">
		<div role="tablist" aria-label="Demo">
			${tabs}
		</div>
		${panels}
	</div>`;
}

function makeStorage() {
	const store = new Map<string, string>();
	return {
		clear() {
			store.clear();
		},
		getItem(key: string) {
			return store.get(key) ?? null;
		},
		key(index: number) {
			return Array.from(store.keys())[index] ?? null;
		},
		removeItem(key: string) {
			store.delete(key);
		},
		setItem(key: string, value: string) {
			store.set(key, value);
		},
		get length() {
			return store.size;
		},
	};
}

describe("code tabs", () => {
	beforeEach(() => {
		document.body.innerHTML = makeTabsMarkup();
		Object.defineProperty(window, "localStorage", {
			value: makeStorage(),
			configurable: true,
		});
		window.localStorage.clear();
	});

	it("defaults to Musi when no preference is stored", () => {
		initCodeTabs();
		const root = document.querySelector<HTMLElement>("[data-code-tabs='1']");
		expect((root?.dataset as TestTabsDataset | undefined)?.activeLanguage).toBe(
			"musi",
		);
		expect(
			root
				?.querySelector('[role="tabpanel"][data-language="musi"]')
				?.hasAttribute("hidden"),
		).toBe(false);
	});

	it("persists selected language", () => {
		initCodeTabs();
		const tab = document.querySelector<HTMLElement>(
			'[role="tab"][data-language="csharp"]',
		);
		tab?.click();
		expect(window.localStorage.getItem("musi-code-lang")).toBe("csharp");
	});

	it("upgrades an existing block idempotently", () => {
		initCodeTabs();
		initCodeTabs();
		const root = document.querySelector<HTMLElement>("[data-code-tabs='1']");
		expect((root?.dataset as TestTabsDataset | undefined)?.upgraded).toBe(
			"true",
		);
	});

	it("binds fresh tab markup after same-page html replacement", () => {
		initCodeTabs();
		const mount = document.createElement("div");
		document.body.innerHTML = "";
		mount.innerHTML = makeTabsMarkup();
		document.body.appendChild(mount);

		initCodeTabsWithin(mount);

		const tab = mount.querySelector<HTMLElement>(
			'[role="tab"][data-language="typescript"]',
		);
		tab?.click();

		const root = mount.querySelector<HTMLElement>("[data-code-tabs='1']");
		expect((root?.dataset as TestTabsDataset | undefined)?.activeLanguage).toBe(
			"typescript",
		);
		expect(window.localStorage.getItem("musi-code-lang")).toBe("typescript");
	});
});
