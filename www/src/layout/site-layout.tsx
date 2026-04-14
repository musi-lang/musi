import type { ReactNode } from "react";
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

export type ColorScheme = "light" | "dark" | "system";

const schemeOrder: readonly ColorScheme[] = ["light", "dark", "system"];

export function nextScheme(current: ColorScheme): ColorScheme {
	const index = schemeOrder.indexOf(current);
	return schemeOrder[(index + 1) % schemeOrder.length] ?? "system";
}

function HeaderLinks(props: { route: AppRoute }) {
	return (
		<>
			{primaryRoutes
				.filter((route) => route.locale === props.route.locale)
				.map((route) => (
					<a
						key={route.id}
						href={route.path}
						className={`header-link${props.route.section === route.section ? " is-active" : ""}`}
					>
						{route.label}
					</a>
				))}
		</>
	);
}

function ThemeControl(props: { locale: Locale }) {
	const labels = siteCopy[props.locale].utilityLabels;
	return (
		<ThemeToggleButton
			aria-label={labels.themeSystem}
			title={labels.themeSystem}
			data-theme-toggle={true}
			data-label-light={labels.themeLight}
			data-label-dark={labels.themeDark}
			data-label-system={labels.themeSystem}
			data-scheme="system"
		>
			<span data-theme-icon="light" hidden={true} aria-hidden="true">
				<SunIcon size={18} />
			</span>
			<span data-theme-icon="dark" hidden={true} aria-hidden="true">
				<MoonIcon size={18} />
			</span>
			<span data-theme-icon="system" aria-hidden="true">
				<DesktopIcon size={18} />
			</span>
			<span className="sr-only theme-toggle-label">{labels.themeSystem}</span>
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

	return (
		<details className="locale-menu">
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
					className={`locale-option${currentLocale === "en" ? " is-active" : ""}`}
					lang="en-US"
				>
					<UnitedStatesFlagIcon size={18} />
					<span>English</span>
				</a>
				<a
					href={localeTargetPath(props.route, "ja")}
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

function DocsSidebar(props: { route: AppRoute }) {
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
							className={`docs-sidebar-heading${props.route.path === group.path ? " is-active" : ""}`}
						>
							{group.group}
						</a>
						<div className="docs-sidebar-list">
							{group.pages.map((page) => (
								<a
									key={`${page.locale}:${page.slug}`}
									href={page.path}
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
	const docsMode = isDocsRoute(props.route);
	const copy = siteCopy[props.route.locale];
	const homeHref = props.route.locale === "ja" ? "/ja" : "/";
	const drawerId = docsMode ? "docs-sidebar-drawer" : "site-nav-drawer";

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
							aria-label={copy.menu}
							title={copy.menu}
							aria-expanded="false"
							aria-controls={drawerId}
							data-menu-toggle={true}
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
				<div id={drawerId} className="site-drawer">
					<div className="site-drawer-panel">
						{docsMode ? (
							<DocsSidebar route={props.route} />
						) : (
							<nav aria-label="Primary" className="site-header-nav">
								<HeaderLinks route={props.route} />
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
