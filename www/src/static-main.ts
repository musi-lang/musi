type ColorScheme = "light" | "dark" | "system";
type ResolvedColorScheme = "light" | "dark";
type RootDataset = DOMStringMap & {
	boundThemeMedia?: string;
	colorScheme?: string;
	mantineColorScheme?: string;
};
type ThemeButtonDataset = DOMStringMap & {
	boundThemeToggle?: string;
	labelDark?: string;
	labelLight?: string;
	labelSystem?: string;
	scheme?: ColorScheme;
};
type ThemeIconDataset = DOMStringMap & {
	themeIcon?: string;
};
type MenuButtonDataset = DOMStringMap & {
	boundMenuToggle?: string;
};
type CodeTabTriggerDataset = DOMStringMap & {
	boundCodeTab?: string;
	codeTabTrigger?: string;
};
type CodeTabPanelDataset = DOMStringMap & {
	codeTabPanel?: string;
};

const STORAGE_KEY = "musi-color-scheme";
const MEDIA_QUERY = "(prefers-color-scheme: dark)";
const schemeOrder: readonly ColorScheme[] = ["light", "dark", "system"];

function isColorScheme(value: string | null): value is ColorScheme {
	return value === "light" || value === "dark" || value === "system";
}

function readStoredColorScheme(): ColorScheme {
	const value = window.localStorage.getItem(STORAGE_KEY);
	return isColorScheme(value) ? value : "system";
}

function resolveColorScheme(colorScheme: ColorScheme): ResolvedColorScheme {
	if (colorScheme !== "system") {
		return colorScheme;
	}
	return window.matchMedia(MEDIA_QUERY).matches ? "dark" : "light";
}

function applyColorScheme(
	colorScheme: ColorScheme,
	resolvedColorScheme: ResolvedColorScheme,
) {
	const root = document.documentElement;
	const dataset = root.dataset as RootDataset;
	dataset.colorScheme = colorScheme;
	dataset.mantineColorScheme = resolvedColorScheme;
	root.style.colorScheme = resolvedColorScheme;
}

function nextScheme(current: ColorScheme) {
	const currentIndex = schemeOrder.indexOf(current);
	return schemeOrder[(currentIndex + 1) % schemeOrder.length] ?? "system";
}

function updateThemeToggle(
	button: HTMLButtonElement,
	colorScheme: ColorScheme,
) {
	const dataset = button.dataset as ThemeButtonDataset;
	const labelLight = dataset.labelLight ?? "";
	const labelDark = dataset.labelDark ?? "";
	const labelSystem = dataset.labelSystem ?? "";
	const label =
		colorScheme === "light"
			? labelLight
			: colorScheme === "dark"
				? labelDark
				: labelSystem;
	dataset.scheme = colorScheme;
	button.setAttribute("aria-label", label);
	button.title = label;
	const srLabel = button.querySelector(".theme-toggle-label");
	if (srLabel) {
		srLabel.textContent = label;
	}
	for (const icon of button.querySelectorAll<HTMLElement>(
		"[data-theme-icon]",
	)) {
		icon.hidden = (icon.dataset as ThemeIconDataset).themeIcon !== colorScheme;
	}
}

function setupThemeToggle() {
	const themeButtons = document.querySelectorAll<HTMLButtonElement>(
		"[data-theme-toggle]",
	);
	for (const button of themeButtons) {
		const dataset = button.dataset as ThemeButtonDataset;
		updateThemeToggle(button, readStoredColorScheme());
		if (dataset.boundThemeToggle === "true") {
			continue;
		}
		dataset.boundThemeToggle = "true";
		button.addEventListener("click", () => {
			const buttonScheme = dataset.scheme ?? null;
			const currentScheme = isColorScheme(buttonScheme)
				? buttonScheme
				: readStoredColorScheme();
			const nextColorScheme = nextScheme(currentScheme);
			window.localStorage.setItem(STORAGE_KEY, nextColorScheme);
			applyColorScheme(nextColorScheme, resolveColorScheme(nextColorScheme));
			updateThemeToggle(button, nextColorScheme);
		});
	}

	const mediaQuery = window.matchMedia(MEDIA_QUERY);
	const rootDataset = document.documentElement.dataset as RootDataset;
	if (rootDataset.boundThemeMedia === "true") {
		return;
	}
	rootDataset.boundThemeMedia = "true";
	mediaQuery.addEventListener("change", () => {
		const currentScheme = readStoredColorScheme();
		applyColorScheme(currentScheme, resolveColorScheme(currentScheme));
		for (const button of themeButtons) {
			updateThemeToggle(button, currentScheme);
		}
	});
}

function setupMenuToggle() {
	for (const button of document.querySelectorAll<HTMLButtonElement>(
		"[data-menu-toggle]",
	)) {
		const controlsId = button.getAttribute("aria-controls");
		if (!controlsId) {
			continue;
		}
		const drawer = document.getElementById(controlsId);
		if (!drawer) {
			continue;
		}
		const dataset = button.dataset as MenuButtonDataset;
		if (dataset.boundMenuToggle === "true") {
			continue;
		}
		dataset.boundMenuToggle = "true";
		button.addEventListener("click", () => {
			const expanded = button.getAttribute("aria-expanded") === "true";
			button.setAttribute("aria-expanded", expanded ? "false" : "true");
			drawer.classList.toggle("is-open", !expanded);
		});
	}
}

function selectCodeTab(root: HTMLElement, nextTabId: string) {
	for (const trigger of root.querySelectorAll<HTMLButtonElement>(
		"[data-code-tab-trigger]",
	)) {
		const selected =
			(trigger.dataset as CodeTabTriggerDataset).codeTabTrigger === nextTabId;
		trigger.setAttribute("aria-selected", selected ? "true" : "false");
		trigger.tabIndex = selected ? 0 : -1;
		trigger.classList.toggle("is-active", selected);
	}
	for (const panel of root.querySelectorAll<HTMLElement>(
		"[data-code-tab-panel]",
	)) {
		panel.hidden =
			(panel.dataset as CodeTabPanelDataset).codeTabPanel !== nextTabId;
	}
}

function setupCodeTabs() {
	for (const root of document.querySelectorAll<HTMLElement>(
		"[data-code-tabs]",
	)) {
		for (const trigger of root.querySelectorAll<HTMLButtonElement>(
			"[data-code-tab-trigger]",
		)) {
			const dataset = trigger.dataset as CodeTabTriggerDataset;
			if (dataset.boundCodeTab === "true") {
				continue;
			}
			dataset.boundCodeTab = "true";
			trigger.addEventListener("click", () => {
				const nextTabId = dataset.codeTabTrigger;
				if (nextTabId) {
					selectCodeTab(root, nextTabId);
				}
			});
		}
	}
}

export function setupSiteInteractions() {
	const currentScheme = readStoredColorScheme();
	applyColorScheme(currentScheme, resolveColorScheme(currentScheme));
	setupThemeToggle();
	setupMenuToggle();
	setupCodeTabs();
}
