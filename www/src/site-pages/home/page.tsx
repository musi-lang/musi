import { homeSampleHtml } from "../../content";
import { docGroups, docSearchEntries } from "../../docs";
import { siteCopy } from "../../lib/site-copy";
import type { AppRoute } from "../../routes";
import {
	ActionStrip,
	InlineAction,
	PrimaryAction,
	SecondaryAction,
} from "../../ui/actions";
import { DocListGroup } from "../../ui/doc-list";
import { DocsSearch } from "../../ui/docs-search";
import { HtmlSnippet } from "../../ui/html-snippet";
import { Surface } from "../../ui/surface";

export function HomePage(_props: { route: AppRoute }) {
	const copy = siteCopy.home;
	return (
		<div className="page-stack page-home">
			<section className="home-board" aria-labelledby="home-title">
				<div className="home-board-titlebar">
					<span>{copy.eyebrow}</span>
				</div>
				<div className="home-board-grid">
					<div className="home-board-cell home-board-main">
						<h1
							id="home-title"
							className="page-header-title page-header-title-page"
						>
							{copy.title}
						</h1>
						<div className="page-copy page-header-copy">{copy.description}</div>
						<ul className="hero-summary-list">
							{copy.summary.map((item) => (
								<li key={item} className="hero-summary-item">
									{item}
								</li>
							))}
						</ul>
						<ActionStrip>
							<PrimaryAction href="/learn/book">
								{copy.primaryCta}
							</PrimaryAction>
							<SecondaryAction href="/install">
								{copy.secondaryCta}
							</SecondaryAction>
							<InlineAction href="/community">{copy.tertiaryCta}</InlineAction>
						</ActionStrip>
					</div>
					<div className="home-board-cell home-board-sample">
						<div className="home-board-subtitle">{siteCopy.ui.sample}</div>
						<HtmlSnippet className="docs-content" html={homeSampleHtml} />
					</div>
				</div>
			</section>

			<section className="status-strip" aria-label={siteCopy.ui.status}>
				{copy.statusItems.map((item) => (
					<Surface key={item.label} tone="panel" className="status-card">
						<div className="doc-row-meta">{item.label}</div>
						<strong>{item.value}</strong>
						<span>{item.copy}</span>
					</Surface>
				))}
			</section>

			<section className="portal-board" aria-label={siteCopy.ui.primaryPaths}>
				<div className="section-heading-row section-heading-bar">
					<div>
						<div className="eyebrow">{siteCopy.ui.primaryPaths}</div>
						<h2>Choose a path</h2>
					</div>
				</div>
				<div className="portal-grid">
					{copy.taskBoards.map((link) => (
						<Surface
							key={link.title}
							tone="raised"
							className="portal-card portal-card-raised portal-card-task"
						>
							<div className="eyebrow">{link.label}</div>
							<h2>{link.title}</h2>
							<p>{link.copy}</p>
							<InlineAction href={link.href}>{link.actionLabel}</InlineAction>
						</Surface>
					))}
				</div>
			</section>

			<Surface tone="well" className="section-panel section-panel-structured">
				<DocsSearch
					entries={docSearchEntries}
					heading={siteCopy.ui.findDocs}
					lede="Search chapters, language guides, effects, types, and command questions."
					initialLimit={4}
					queryLimit={8}
				/>
			</Surface>

			<section className="portal-grid" aria-label={siteCopy.ui.primaryPaths}>
				{copy.paths.map((link) => (
					<Surface
						key={link.title}
						tone="raised"
						className="portal-card portal-card-raised"
					>
						<div className="eyebrow">{link.label}</div>
						<h2>{link.title}</h2>
						<p>{link.copy}</p>
						<InlineAction href={link.href}>{link.actionLabel}</InlineAction>
					</Surface>
				))}
			</section>

			<Surface
				tone="well"
				className="section-panel section-panel-structured why-panel"
			>
				<div className="section-heading-row section-heading-bar">
					<div>
						<div className="eyebrow">{siteCopy.ui.learnSection}</div>
						<h2>{copy.sectionsTitle}</h2>
					</div>
				</div>
				<div className="feature-grid feature-grid-separated">
					{copy.sections.map((section) => (
						<Surface
							key={section.title}
							tone="raised"
							className="feature-card feature-card-raised"
						>
							<div className="eyebrow">{section.title}</div>
							<p>{section.copy}</p>
						</Surface>
					))}
				</div>
			</Surface>

			<Surface
				tone="well"
				className="section-panel section-panel-structured learn-map-panel"
			>
				<div className="section-heading-row section-heading-bar">
					<div>
						<div className="eyebrow">{siteCopy.ui.learnSection}</div>
						<h2>{siteCopy.learn.partsTitle}</h2>
					</div>
					<InlineAction href="/learn/book">{siteCopy.nav.learn}</InlineAction>
				</div>
				<div className="doc-groups-grid doc-groups-grid-separated">
					{docGroups.map((group) => (
						<DocListGroup
							key={group.group}
							group={group.group}
							path={group.path}
							summaryHtml={group.summaryHtml}
							pages={group.pages}
							linkLabel={siteCopy.ui.openSection}
						/>
					))}
				</div>
			</Surface>
		</div>
	);
}
