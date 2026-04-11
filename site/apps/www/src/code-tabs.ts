const STORAGE_KEY = "musi-code-lang";
const LANGUAGE_ORDER = ["java", "musi", "rust", "typescript"] as const;

type ComparisonLanguage = (typeof LANGUAGE_ORDER)[number];

interface CodeTabsDataset extends DOMStringMap {
	activeLanguage?: ComparisonLanguage;
	upgraded?: string;
}

interface LanguageDataset extends DOMStringMap {
	active?: string;
	language?: ComparisonLanguage;
}

function isComparisonLanguage(
	value: string | null,
): value is ComparisonLanguage {
	return value !== null && LANGUAGE_ORDER.includes(value as ComparisonLanguage);
}

function preferredLanguage() {
	const stored = window.localStorage.getItem(STORAGE_KEY);
	return isComparisonLanguage(stored) ? stored : "musi";
}

function panelSelector(language: ComparisonLanguage) {
	return `[role="tabpanel"][data-language="${language}"]`;
}

function tabSelector(language: ComparisonLanguage) {
	return `[role="tab"][data-language="${language}"]`;
}

function setTabState(
	root: HTMLElement,
	language: ComparisonLanguage,
	active: boolean,
) {
	const tab = root.querySelector<HTMLElement>(tabSelector(language));
	if (!tab) {
		return;
	}
	const dataset = tab.dataset as LanguageDataset;

	tab.setAttribute("aria-selected", active ? "true" : "false");
	tab.tabIndex = active ? 0 : -1;
	dataset.active = active ? "true" : "false";
}

function setPanelState(
	root: HTMLElement,
	language: ComparisonLanguage,
	active: boolean,
) {
	const panel = root.querySelector<HTMLElement>(panelSelector(language));
	if (!panel) {
		return;
	}
	const dataset = panel.dataset as LanguageDataset;

	panel.hidden = !active;
	dataset.active = active ? "true" : "false";
}

function applyLanguage(root: HTMLElement, language: ComparisonLanguage) {
	const dataset = root.dataset as CodeTabsDataset;
	dataset.activeLanguage = language;
	for (const candidate of LANGUAGE_ORDER) {
		const active = candidate === language;
		setTabState(root, candidate, active);
		setPanelState(root, candidate, active);
	}
}

function moveFocus(
	root: HTMLElement,
	current: ComparisonLanguage,
	delta: number,
) {
	const index = LANGUAGE_ORDER.indexOf(current);
	const next =
		LANGUAGE_ORDER[
			(index + delta + LANGUAGE_ORDER.length) % LANGUAGE_ORDER.length
		] ?? "musi";
	const tab = root.querySelector<HTMLElement>(tabSelector(next));
	if (tab) {
		tab.focus();
		applyLanguage(root, next);
		window.localStorage.setItem(STORAGE_KEY, next);
	}
}

function upgradeCodeTabs(root: HTMLElement) {
	const dataset = root.dataset as CodeTabsDataset;
	if (dataset.upgraded === "true") {
		applyLanguage(root, preferredLanguage());
		return;
	}

	dataset.upgraded = "true";
	const initial = preferredLanguage();
	applyLanguage(root, initial);

	root.addEventListener("click", (event) => {
		const target = event.target;
		if (!(target instanceof HTMLElement)) {
			return;
		}
		const tab = target.closest<HTMLElement>('[role="tab"][data-language]');
		if (!tab) {
			return;
		}
		const language = (tab.dataset as LanguageDataset).language;
		if (!isComparisonLanguage(language ?? null)) {
			return;
		}
		const nextLanguage = language as ComparisonLanguage;
		applyLanguage(root, nextLanguage);
		window.localStorage.setItem(STORAGE_KEY, nextLanguage);
	});

	root.addEventListener("keydown", (event) => {
		const target = event.target;
		if (
			!(target instanceof HTMLElement) ||
			target.getAttribute("role") !== "tab"
		) {
			return;
		}
		const current = (target.dataset as LanguageDataset).language;
		if (!isComparisonLanguage(current ?? null)) {
			return;
		}
		const currentLanguage = current as ComparisonLanguage;

		switch (event.key) {
			case "ArrowLeft":
			case "ArrowUp":
				event.preventDefault();
				moveFocus(root, currentLanguage, -1);
				break;
			case "ArrowRight":
			case "ArrowDown":
				event.preventDefault();
				moveFocus(root, currentLanguage, 1);
				break;
			case "Home":
				event.preventDefault();
				applyLanguage(root, "java");
				window.localStorage.setItem(STORAGE_KEY, "java");
				root.querySelector<HTMLElement>(tabSelector("java"))?.focus();
				break;
			case "End":
				event.preventDefault();
				applyLanguage(root, "typescript");
				window.localStorage.setItem(STORAGE_KEY, "typescript");
				root.querySelector<HTMLElement>(tabSelector("typescript"))?.focus();
				break;
			default:
				break;
		}
	});
}

export function initCodeTabsWithin(root: ParentNode) {
	for (const element of root.querySelectorAll<HTMLElement>(
		"[data-code-tabs='1']",
	)) {
		upgradeCodeTabs(element);
	}
}

export function initCodeTabs() {
	initCodeTabsWithin(document);
}
