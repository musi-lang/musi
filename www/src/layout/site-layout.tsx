import type { ReactNode } from "preact/compat";
import { type DocPage, docChildren, docForPath, docParts } from "../docs";
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
	const activeDoc = docForPath(props.route.path);
	const activeTreePath = new Set(activeDoc?.treePath ?? []);
	const renderSidebarNode = (node: DocPage, depth: number) => {
		const children = docChildren(node.id);
		const levelClass =
			node.kind === "section"
				? "docs-sidebar-link-section"
				: "docs-sidebar-link-chapter";
		return (
			<div key={node.id} className="docs-sidebar-node">
				<a
					href={node.path}
					className={`docs-sidebar-link ${levelClass}${activeTreePath.has(node.id) ? " is-active" : ""}`}
					data-depth={depth}
				>
					{node.title}
				</a>
				{children.length > 0 ? (
					<div className="docs-sidebar-children">
						{children.map((child) => renderSidebarNode(child, depth + 1))}
					</div>
				) : null}
			</div>
		);
	};

	return (
		<nav aria-label="Documentation" className="docs-sidebar-nav">
			{docParts.map((part) => (
				<section key={part.id} className="docs-sidebar-group">
					<a
						href={part.path}
						className={`docs-sidebar-heading${activeTreePath.has(part.id) ? " is-active" : ""}`}
					>
						{part.title}
					</a>
					<div className="docs-sidebar-list">
						{docChildren(part.id).map((node) => renderSidebarNode(node, 0))}
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
							<span className="site-logo-title">MUSI</span>
						</a>
					</div>
					<div className="site-header-actions">
						<a
							href="https://github.com/musi-lang/musi"
							target="_blank"
							rel="noreferrer"
							className="header-icon-control utility-icon"
							aria-label={siteCopy.utilityLabels.github}
							title={siteCopy.utilityLabels.github}
						>
							<GithubIcon size={18} />
							<span className="sr-only">{siteCopy.utilityLabels.github}</span>
						</a>
						<ThemeControl />
					</div>
				</div>
				<nav
					aria-label="Primary"
					className="site-header-tabbar site-header-nav site-header-nav-desktop"
				>
					<HeaderLinks route={props.route} />
				</nav>
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
