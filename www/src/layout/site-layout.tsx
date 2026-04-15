import type { ReactNode } from "react";
import { docGroups } from "../docs";
import {
	DesktopIcon,
	GithubIcon,
	MenuIcon,
	MoonIcon,
	MusiMarkIcon,
	SunIcon,
} from "../icons";
import { siteCopy } from "../lib/site-copy";
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
			{primaryRoutes.map((route) => (
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

function ThemeControl() {
	const labels = siteCopy.utilityLabels;
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

function DocsSidebar(props: { route: AppRoute }) {
	return (
		<nav aria-label="Documentation" className="docs-sidebar-nav">
			{docGroups.map((group) => (
				<section key={group.group} className="docs-sidebar-group">
					<a
						href={group.path}
						className={`docs-sidebar-heading${props.route.path === group.path ? " is-active" : ""}`}
					>
						{group.group}
					</a>
					<div className="docs-sidebar-list">
						{group.pages.map((page) => (
							<a
								key={page.slug}
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
	const drawerId = docsMode ? "docs-sidebar-drawer" : "site-nav-drawer";

	return (
		<div className={`site-shell${docsMode ? " site-shell-docs" : ""}`}>
			<a href="#main-content" className="skip-link">
				{siteCopy.skipToContent}
			</a>
			<header className="site-header">
				<div className="site-header-inner">
					<div className="site-brand-row">
						<button
							type="button"
							className="menu-toggle"
							aria-label={siteCopy.menu}
							title={siteCopy.menu}
							aria-expanded="false"
							aria-controls={drawerId}
							data-menu-toggle={true}
						>
							<MenuIcon size={18} />
							<span className="sr-only">{siteCopy.menu}</span>
						</button>
						<a href="/" className="site-logo">
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
							aria-label={siteCopy.utilityLabels.github}
							title={siteCopy.utilityLabels.github}
						>
							<GithubIcon size={18} />
							<span className="sr-only">{siteCopy.utilityLabels.github}</span>
						</a>
						<ThemeControl />
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
