import {
	docBreadcrumb,
	docChildren,
	docForPath,
	docGroups,
	docNeighbors,
	docSearchEntries,
} from "../../docs";
import { siteCopy } from "../../lib/site-copy";
import type { AppRoute } from "../../routes";
import { InlineAction, SecondaryAction } from "../../ui/actions";
import { DocListGroup } from "../../ui/doc-list";
import { DocsSearch } from "../../ui/docs-search";
import { HtmlSnippet } from "../../ui/html-snippet";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";
import { OnThisPage } from "../../ui/toc";

export function DocsIndexPage(_props: { route: AppRoute }) {
	const copy = siteCopy.learn;
	return (
		<div className="page-stack">
			<PageHeader
				eyebrow={copy.eyebrow}
				title={copy.title}
				description={copy.description}
				actions={
					<div className="action-strip">
						<SecondaryAction href="/install">
							{siteCopy.nav.install}
						</SecondaryAction>
						<InlineAction href="/community">
							{siteCopy.nav.community}
						</InlineAction>
					</div>
				}
			/>
			<section className="portal-grid" aria-label={siteCopy.ui.docsEntryPoints}>
				<Surface tone="accent" className="portal-card portal-card-task">
					<div className="eyebrow">{copy.eyebrow}</div>
					<h2>{copy.startTitle}</h2>
					<p>{copy.description}</p>
					<InlineAction href="/learn/book/start/getting-started">
						{siteCopy.ui.openFirstChapter}
					</InlineAction>
				</Surface>
				<Surface tone="panel" className="portal-card portal-card-task">
					<div className="eyebrow">{siteCopy.ui.learnSection}</div>
					<h2>{copy.partsTitle}</h2>
					<p>
						Read short chapters in order and move forward only when the current
						mental model feels stable.
					</p>
				</Surface>
			</section>
			<Surface tone="well" className="section-panel section-panel-structured">
				<DocsSearch
					entries={docSearchEntries}
					heading={siteCopy.ui.findDocs}
					lede="Type a topic, command, source language, or question."
				/>
			</Surface>
			<Surface tone="panel" className="section-panel">
				<div className="section-heading-row">
					<div>
						<div className="eyebrow">{siteCopy.ui.learnSection}</div>
						<h2>{copy.partsTitle}</h2>
					</div>
				</div>
				<div className="doc-groups-grid doc-groups-grid-compact">
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
			<Surface tone="panel" className="section-panel">
				<div className="section-heading-row">
					<div>
						<div className="eyebrow">{siteCopy.ui.learnSection}</div>
						<h2>{copy.startTitle}</h2>
					</div>
				</div>
				<p className="muted">
					Musi Book is the main reading path. Use the chapter list when you want
					structure, or jump into a developer guide when you are mapping from
					another language.
				</p>
			</Surface>
		</div>
	);
}

function SectionNav(props: {
	previous: { path: string; title: string } | undefined;
	next: { path: string; title: string } | undefined;
}) {
	if (!(props.previous || props.next)) {
		return null;
	}
	const labels = siteCopy.ui;
	return (
		<nav className="section-nav" aria-label={labels.chapterNavigation}>
			<div>
				{props.previous ? (
					<InlineAction href={props.previous.path}>
						<span aria-hidden="true">&lt;&lt;</span>
						<span>
							{labels.previousChapter}: {props.previous.title}
						</span>
					</InlineAction>
				) : null}
			</div>
			<div>
				{props.next ? (
					<InlineAction href={props.next.path}>
						<span>
							{labels.nextChapter}: {props.next.title}
						</span>
						<span aria-hidden="true">&gt;&gt;</span>
					</InlineAction>
				) : null}
			</div>
		</nav>
	);
}

export function DocPage(props: { pathname: string; route: AppRoute }) {
	const page = docForPath(props.pathname);
	if (!page) {
		return null;
	}
	const neighbors = page.kind === "chapter" ? docNeighbors(page.id) : {};
	const childPages =
		page.kind === "part" || page.kind === "section" ? docChildren(page.id) : [];
	const breadcrumb = docBreadcrumb(page.id);
	return (
		<div className="page-stack docs-page">
			<PageHeader
				eyebrow={
					<span className="crumbs">
						<a href="/learn">{siteCopy.nav.learn}</a>
						{breadcrumb.map((node, index) => (
							<span key={node.id} className="crumb-node">
								<span aria-hidden="true">/</span>
								{index === breadcrumb.length - 1 ? (
									<span>{node.title}</span>
								) : (
									<a href={node.path}>{node.title}</a>
								)}
							</span>
						))}
					</span>
				}
				title={page.title}
				descriptionHtml={page.descriptionHtml}
			/>
			<OnThisPage
				headings={page.headings}
				className="toc-panel-mobile"
				label={siteCopy.ui.onThisPage}
			/>
			<Surface tone="well" className="section-panel docs-page-tools">
				<DocsSearch
					entries={docSearchEntries}
					heading={siteCopy.ui.findDocs}
					lede="Jump to another chapter without losing the current reading path."
					variant="disclosure"
				/>
			</Surface>
			<div className="docs-body-grid">
				<Surface tone="base" className="doc-article-surface">
					<article className="docs-article">
						<HtmlSnippet className="docs-content" html={page.html} />
						{childPages.length > 0 ? (
							<div className="part-children-block">
								<div className="eyebrow">{siteCopy.ui.chapters}</div>
								<DocListGroup group={page.title} pages={childPages} />
							</div>
						) : null}
					</article>
				</Surface>
				<OnThisPage
					headings={page.headings}
					className="toc-panel-desktop"
					label={siteCopy.ui.onThisPage}
				/>
			</div>
			{page.kind === "chapter" ? (
				<SectionNav previous={neighbors.previous} next={neighbors.next} />
			) : null}
		</div>
	);
}
