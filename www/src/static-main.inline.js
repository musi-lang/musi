(() => {
	const storageKey = "musi-color-scheme";
	const mediaQueryText = "(prefers-color-scheme: dark)";
	const schemeOrder = ["light", "dark", "system"];

	function isColorScheme(value) {
		return value === "light" || value === "dark" || value === "system";
	}

	function readStoredColorScheme() {
		const value = window.localStorage.getItem(storageKey);
		return isColorScheme(value) ? value : "system";
	}

	function resolveColorScheme(colorScheme) {
		if (colorScheme !== "system") {
			return colorScheme;
		}
		return window.matchMedia(mediaQueryText).matches ? "dark" : "light";
	}

	function applyColorScheme(colorScheme, resolvedColorScheme) {
		const root = document.documentElement;
		root.dataset.colorScheme = colorScheme;
		root.dataset.mantineColorScheme = resolvedColorScheme;
		root.style.colorScheme = resolvedColorScheme;
	}

	function nextScheme(current) {
		const currentIndex = schemeOrder.indexOf(current);
		return schemeOrder[(currentIndex + 1) % schemeOrder.length] ?? "system";
	}

	function updateThemeToggle(button, colorScheme) {
		const labelLight = button.dataset.labelLight ?? "";
		const labelDark = button.dataset.labelDark ?? "";
		const labelSystem = button.dataset.labelSystem ?? "";
		const label =
			colorScheme === "light"
				? labelLight
				: colorScheme === "dark"
					? labelDark
					: labelSystem;
		button.dataset.scheme = colorScheme;
		button.setAttribute("aria-label", label);
		button.title = label;
		const srLabel = button.querySelector(".theme-toggle-label");
		if (srLabel) {
			srLabel.textContent = label;
		}
		for (const icon of button.querySelectorAll("[data-theme-icon]")) {
			icon.hidden = icon.dataset.themeIcon !== colorScheme;
		}
	}

	function setupThemeToggle() {
		const themeButtons = document.querySelectorAll("[data-theme-toggle]");
		for (const button of themeButtons) {
			updateThemeToggle(button, readStoredColorScheme());
			if (button.dataset.boundThemeToggle === "true") {
				continue;
			}
			button.dataset.boundThemeToggle = "true";
			button.addEventListener("click", () => {
				const buttonScheme = button.dataset.scheme ?? null;
				const currentScheme = isColorScheme(buttonScheme)
					? buttonScheme
					: readStoredColorScheme();
				const nextColorScheme = nextScheme(currentScheme);
				window.localStorage.setItem(storageKey, nextColorScheme);
				applyColorScheme(nextColorScheme, resolveColorScheme(nextColorScheme));
				updateThemeToggle(button, nextColorScheme);
			});
		}

		const mediaQuery = window.matchMedia(mediaQueryText);
		if (document.documentElement.dataset.boundThemeMedia === "true") {
			return;
		}
		document.documentElement.dataset.boundThemeMedia = "true";
		mediaQuery.addEventListener("change", () => {
			const currentScheme = readStoredColorScheme();
			applyColorScheme(currentScheme, resolveColorScheme(currentScheme));
			for (const button of themeButtons) {
				updateThemeToggle(button, currentScheme);
			}
		});
	}

	function setupMenuToggle() {
		for (const button of document.querySelectorAll("[data-menu-toggle]")) {
			const controlsId = button.getAttribute("aria-controls");
			if (!controlsId) {
				continue;
			}
			const drawer = document.getElementById(controlsId);
			if (!drawer || button.dataset.boundMenuToggle === "true") {
				continue;
			}
			button.dataset.boundMenuToggle = "true";
			button.addEventListener("click", () => {
				const expanded = button.getAttribute("aria-expanded") === "true";
				button.setAttribute("aria-expanded", expanded ? "false" : "true");
				drawer.classList.toggle("is-open", !expanded);
			});
		}
	}

	function selectCodeTab(root, nextTabId) {
		for (const trigger of root.querySelectorAll("[data-code-tab-trigger]")) {
			const selected = trigger.dataset.codeTabTrigger === nextTabId;
			trigger.setAttribute("aria-selected", selected ? "true" : "false");
			trigger.tabIndex = selected ? 0 : -1;
			trigger.classList.toggle("is-active", selected);
		}
		for (const panel of root.querySelectorAll("[data-code-tab-panel]")) {
			panel.hidden = panel.dataset.codeTabPanel !== nextTabId;
		}
	}

	function setupCodeTabs() {
		for (const root of document.querySelectorAll("[data-code-tabs]")) {
			for (const trigger of root.querySelectorAll("[data-code-tab-trigger]")) {
				if (trigger.dataset.boundCodeTab === "true") {
					continue;
				}
				trigger.dataset.boundCodeTab = "true";
				trigger.addEventListener("click", () => {
					const nextTabId = trigger.dataset.codeTabTrigger;
					if (nextTabId) {
						selectCodeTab(root, nextTabId);
					}
				});
			}
		}
	}

	function normalizeSearchText(text) {
		return text.trim().toLowerCase();
	}

	function readDocsSearchLimit(root, attributeName, fallback) {
		const limit = Number.parseInt(root.dataset[attributeName] ?? "", 10);
		return Number.isFinite(limit) && limit > 0 ? limit : fallback;
	}

	function applyDocsSearchFilter(root, input, entries, empty) {
		const query = normalizeSearchText(input.value ?? "");
		const fallbackLimit = readDocsSearchLimit(
			root,
			"docsSearchInitialLimit",
			8,
		);
		const limit =
			query === ""
				? fallbackLimit
				: readDocsSearchLimit(root, "docsSearchQueryLimit", fallbackLimit);
		let visibleCount = 0;
		for (const entry of entries) {
			const matches =
				query === "" || (entry.dataset.searchText ?? "").includes(query);
			const visible = matches && visibleCount < limit;
			entry.hidden = !visible;
			if (visible) {
				visibleCount += 1;
			}
		}
		if (empty) {
			empty.hidden = visibleCount > 0;
		}
	}

	function closeDocsSearchDisclosure(details, summary) {
		details.open = false;
		if (summary) {
			summary.focus();
		}
	}

	function setupDocsSearchDisclosure(root, details, entries) {
		const summary = details.querySelector("summary");
		document.addEventListener("click", (event) => {
			const targetNode = event.target;
			if (
				details.open &&
				targetNode instanceof Node &&
				!details.contains(targetNode)
			) {
				details.open = false;
			}
		});
		document.addEventListener("keydown", (event) => {
			if (event.key === "Escape" && details.open) {
				closeDocsSearchDisclosure(details, summary);
			}
		});
		for (const entry of entries) {
			entry.addEventListener("click", () => {
				details.open = false;
			});
		}
		root.dataset.boundDocsSearchDisclosure = "true";
	}

	function setupDocsSearch() {
		for (const root of document.querySelectorAll("[data-docs-search]")) {
			const input = root.querySelector("[data-docs-search-input]");
			const entries = Array.from(
				root.querySelectorAll("[data-docs-search-entry]"),
			);
			const empty = root.querySelector("[data-docs-search-empty]");
			const details = root.querySelector("[data-docs-search-details]");
			if (!input || input.dataset.boundDocsSearch === "true") {
				continue;
			}
			input.dataset.boundDocsSearch = "true";
			const update = () => {
				applyDocsSearchFilter(root, input, entries, empty);
			};
			input.addEventListener("input", update);
			update();
			if (details && root.dataset.boundDocsSearchDisclosure !== "true") {
				setupDocsSearchDisclosure(root, details, entries);
			}
		}
	}

	function setupSiteInteractions() {
		const currentScheme = readStoredColorScheme();
		applyColorScheme(currentScheme, resolveColorScheme(currentScheme));
		setupThemeToggle();
		setupMenuToggle();
		setupCodeTabs();
		setupDocsSearch();
	}

	window.requestAnimationFrame(setupSiteInteractions);
})();
