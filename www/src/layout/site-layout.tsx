import { type ReactNode, useRef, useState } from "react";
import { docGroups } from "../docs";
import {
	ChevronDownIcon,
	DesktopIcon,
	GithubIcon,
	JapanFlagIcon,
	MenuIcon,
	MoonIcon,
	MusiMarkIcon,
	SunIcon,
	UnitedStatesFlagIcon,
} from "../icons";
import { type Locale, siteCopy } from "../lib/site-copy";
import { localizePath } from "../lib/site-links";
import type { AppRoute } from "../routes";
import { isDocsRoute, primaryRoutes } from "../routes";
import { ThemeToggleButton } from "../ui/actions";
import { type ColorScheme, useTheme } from "../ui/theme";

const schemeOrder: readonly ColorScheme[] = [
	"light",
	"dark",
	"system",
] as const;

export function nextScheme(current: ColorScheme): ColorScheme {
	const index = schemeOrder.indexOf(current);
	return schemeOrder[(index + 1) % schemeOrder.length];
}

function themeLabel(locale: Locale, scheme: ColorScheme) {
	const labels = siteCopy[locale].utilityLabels;
	if (scheme === "light") {
		return labels.themeLight;
	}
	if (scheme === "dark") {
		return labels.themeDark;
	}
	return labels.themeSystem;
}

function HeaderLinks(props: { route: AppRoute; onNavigate?: () => void }) {
	return (
		<>
			{primaryRoutes
				.filter((route) => route.locale === props.route.locale)
				.map((route) => (
					<a
						key={route.id}
						href={route.path}
						onClick={props.onNavigate}
						className={`header-link${props.route.section === route.section ? " is-active" : ""}`}
					>
						{route.label}
					</a>
				))}
		</>
	);
}

function ThemeControl(props: { locale: Locale }) {
	const { colorScheme, setColorScheme } = useTheme();
	const Icon =
		colorScheme === "light"
			? SunIcon
			: colorScheme === "dark"
				? MoonIcon
				: DesktopIcon;
	return (
		<ThemeToggleButton
			aria-label={themeLabel(props.locale, colorScheme)}
			title={themeLabel(props.locale, colorScheme)}
			onClick={() => setColorScheme(nextScheme(colorScheme))}
		>
			<Icon size={18} />
			<span className="sr-only">{themeLabel(props.locale, colorScheme)}</span>
		</ThemeToggleButton>
	);
}

function localeTargetPath(route: AppRoute, locale: Locale) {
	const basePath =
		route.path === "/ja"
			? "/"
			: route.path.startsWith("/ja/")
				? route.path.slice(3)
				: route.path;
	return localizePath(locale, basePath);
}

function LocaleSwitch(props: { route: AppRoute }) {
	const copy = siteCopy[props.route.locale];
	const currentLocale = props.route.locale;
	const CurrentFlag =
		currentLocale === "en" ? UnitedStatesFlagIcon : JapanFlagIcon;
	const menuRef = useRef<HTMLDetailsElement | null>(null);

	function closeMenu() {
		if (menuRef.current) {
			menuRef.current.open = false;
		}
	}

	return (
		<details ref={menuRef} className="locale-menu">
			<summary
				className="header-utility-link locale-link locale-summary"
				aria-label={copy.utilityLabels.locale}
				title={copy.utilityLabels.locale}
			>
				<CurrentFlag size={18} />
				<ChevronDownIcon size={14} />
				<span className="sr-only">{copy.utilityLabels.locale}</span>
			</summary>
			<div className="locale-menu-panel">
				<a
					href={localeTargetPath(props.route, "en")}
					onClick={closeMenu}
					className={`locale-option${currentLocale === "en" ? " is-active" : ""}`}
					lang="en-US"
				>
					<UnitedStatesFlagIcon size={18} />
					<span>English</span>
				</a>
				<a
					href={localeTargetPath(props.route, "ja")}
					onClick={closeMenu}
					className={`locale-option${currentLocale === "ja" ? " is-active" : ""}`}
					lang="ja"
				>
					<JapanFlagIcon size={18} />
					<span>日本語</span>
				</a>
			</div>
		</details>
	);
}

function DocsSidebar(props: { route: AppRoute; onNavigate?: () => void }) {
	return (
		<nav aria-label="Documentation" className="docs-sidebar-nav">
			{docGroups
				.filter((group) => group.locale === props.route.locale)
				.map((group) => (
					<section
						key={`${group.locale}:${group.group}`}
						className="docs-sidebar-group"
					>
						<a
							href={group.path}
							onClick={props.onNavigate}
							className={`docs-sidebar-heading${props.route.path === group.path ? " is-active" : ""}`}
						>
							{group.group}
						</a>
						<div className="docs-sidebar-list">
							{group.pages.map((page) => (
								<a
									key={`${page.locale}:${page.slug}`}
									href={page.path}
									onClick={props.onNavigate}
									className={`docs-sidebar-link${props.route.path === page.path ? " is-active" : ""}`}
								>
									{page.title}
								</a>
							))}
						</div>
					</section>
				))}
		</nav>
	);
}

export function SiteLayout(props: { route: AppRoute; children: ReactNode }) {
	const [menuOpen, setMenuOpen] = useState(false);
	const docsMode = isDocsRoute(props.route);
	const copy = siteCopy[props.route.locale];
	const homeHref = props.route.locale === "ja" ? "/ja" : "/";

	return (
		<div className={`site-shell${docsMode ? " site-shell-docs" : ""}`}>
			<a href="#main-content" className="skip-link">
				{copy.skipToContent}
			</a>
			<header className="site-header">
				<div className="site-header-inner">
					<div className="site-brand-row">
						<button
							type="button"
							className="menu-toggle"
							onClick={() => setMenuOpen((current) => !current)}
							aria-label={copy.menu}
							title={copy.menu}
							aria-expanded={menuOpen}
							aria-controls={
								docsMode ? "docs-sidebar-drawer" : "site-nav-drawer"
							}
						>
							<MenuIcon size={18} />
							<span className="sr-only">{copy.menu}</span>
						</button>
						<a href={homeHref} className="site-logo">
							<span className="site-logo-mark" aria-hidden="true">
								<MusiMarkIcon size={28} />
							</span>
							<span className="site-logo-title">Musi</span>
						</a>
					</div>
					<div className="site-header-actions">
						<nav
							aria-label="Primary"
							className="site-header-nav site-header-nav-desktop"
						>
							<HeaderLinks route={props.route} />
						</nav>
						<a
							href="https://github.com/musi-lang/musi"
							target="_blank"
							rel="noreferrer"
							className="header-utility-link utility-icon"
							aria-label={copy.utilityLabels.github}
							title={copy.utilityLabels.github}
						>
							<GithubIcon size={18} />
							<span className="sr-only">{copy.utilityLabels.github}</span>
						</a>
						<LocaleSwitch route={props.route} />
						<ThemeControl locale={props.route.locale} />
					</div>
				</div>
				<div
					id={docsMode ? "docs-sidebar-drawer" : "site-nav-drawer"}
					className={`site-drawer${menuOpen ? " is-open" : ""}`}
				>
					<div className="site-drawer-panel">
						{docsMode ? (
							<DocsSidebar
								route={props.route}
								onNavigate={() => setMenuOpen(false)}
							/>
						) : (
							<nav aria-label="Primary" className="site-header-nav">
								<HeaderLinks
									route={props.route}
									onNavigate={() => setMenuOpen(false)}
								/>
							</nav>
						)}
					</div>
				</div>
			</header>
			<div className="site-frame">
				{docsMode ? (
					<aside className="docs-sidebar" aria-label="Documentation sections">
						<DocsSidebar route={props.route} />
					</aside>
				) : null}
				<main id="main-content" className="site-main">
					{props.children}
				</main>
			</div>
		</div>
	);
}
