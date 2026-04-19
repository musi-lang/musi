import {
	docBreadcrumb,
	docChildren,
	docForPath,
	docGroups,
	docNeighbors,
	docSearchEntries,
	guideGroups,
} from "../../docs";
import { siteCopy } from "../../lib/site-copy";
import type { AppRoute } from "../../routes";
import { ActionStrip, InlineAction, SecondaryAction } from "../../ui/actions";
import { DocListGroup } from "../../ui/doc-list";
import { DocsSearch } from "../../ui/docs-search";
import { HtmlSnippet } from "../../ui/html-snippet";
import {
	MachinesActionCard,
	MachinesGroupbox,
	MachinesPost,
	MachinesSection,
	MachinesSidebarPanel,
} from "../../ui/machines";
import { SiteHeading } from "../../ui/site-heading";
import { OnThisPageNav } from "../../ui/toc";

export function DocsIndexPage(_props: { route: AppRoute }) {
	const copy = siteCopy.learn;
	return (
		<div className="site-stack">
			<SiteHeading
				eyebrow={copy.eyebrow}
				title={copy.title}
				description={copy.description}
				actions={
					<ActionStrip>
						<SecondaryAction href="/install">
							{siteCopy.nav.install}
						</SecondaryAction>
						<InlineAction href="/community">
							{siteCopy.nav.community}
						</InlineAction>
					</ActionStrip>
				}
			/>
			<section
				className="mx-grid site-grid--two"
				aria-label={siteCopy.ui.docsEntryPoints}
			>
				<MachinesActionCard
					href="/learn/book"
					kicker={copy.eyebrow}
					title="Musi Book"
					description="Read the main Musi book in order, without language-transition guides mixed into the path."
					action="Open book"
				/>
				<MachinesActionCard
					href="/learn/guides"
					kicker="Guides"
					title="Musi for Developers"
					description="Use these when you already know another language and want a translation map."
					action="Open guides"
				/>
			</section>
			<MachinesSection title={siteCopy.ui.findDocs}>
				<DocsSearch
					entries={docSearchEntries}
					heading={siteCopy.ui.findDocs}
					lede="Type a topic, command, source language, or question."
					showHeader={false}
				/>
			</MachinesSection>
			<MachinesSection kicker={siteCopy.ui.learnSection} title="Musi Book">
				<p className="site-copy site-copy--small">
					This is the main book. It moves from first file to syntax, data,
					types, effects, and tooling. Language guides are separate because they
					answer a different question.
				</p>
				<div className="mx-grid site-doc-groups">
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
			</MachinesSection>
			<MachinesSection kicker="Guides" title="Musi for Developers">
				<p className="site-copy site-copy--small">
					Use these pages only when you want to map C, Rust, Python, JavaScript,
					or another known language into Musi terms.
				</p>
				<div className="mx-grid site-doc-groups">
					{guideGroups.map((group) => (
						<DocListGroup
							key={group.group}
							group={group.group}
							path={group.path}
							summaryHtml={group.summaryHtml}
							pages={group.pages}
							linkLabel="Open guides"
						/>
					))}
				</div>
			</MachinesSection>
		</div>
	);
}

function DocPathRail(props: { breadcrumb: ReturnType<typeof docBreadcrumb> }) {
	return (
		<nav aria-label="Current page path" className="docs-rail-path">
			<a href="/learn">{siteCopy.nav.learn}</a>
			{props.breadcrumb.map((node, index) => (
				<span key={node.id} className="docs-rail-path__node">
					<span aria-hidden="true">/</span>
					{index === props.breadcrumb.length - 1 ? (
						<span>{node.title}</span>
					) : (
						<a href={node.path}>{node.title}</a>
					)}
				</span>
			))}
		</nav>
	);
}

function DocChapterActions(props: {
	previous: { path: string; title: string } | undefined;
	next: { path: string; title: string } | undefined;
}) {
	if (!(props.previous || props.next)) {
		return null;
	}
	const labels = siteCopy.ui;
	return (
		<nav className="docs-rail-actions" aria-label={labels.chapterNavigation}>
			{props.previous ? (
				<MachinesActionCard
					href={props.previous.path}
					className="docs-rail-action"
					kicker={labels.previousChapter}
					title={props.previous.title}
					action="‹"
				/>
			) : null}
			{props.next ? (
				<MachinesActionCard
					href={props.next.path}
					className="docs-rail-action"
					kicker={labels.nextChapter}
					title={props.next.title}
					action="›"
				/>
			) : null}
		</nav>
	);
}

function DocPageRail(props: {
	breadcrumb: ReturnType<typeof docBreadcrumb>;
	headings: Array<{ depth: number; id: string; text: string }>;
	previous: { path: string; title: string } | undefined;
	next: { path: string; title: string } | undefined;
}) {
	return (
		<MachinesSidebarPanel className="docs-page-rail" title="Chapter tools">
			<MachinesGroupbox
				legend={siteCopy.ui.onThisPage}
				className="docs-rail-box"
			>
				<OnThisPageNav headings={props.headings} />
			</MachinesGroupbox>
			<MachinesGroupbox legend="Jump to chapter" className="docs-rail-box">
				<DocsSearch
					entries={docSearchEntries}
					heading="Open chapter search"
					inputLabel="Search chapters"
					lede="Jump to another chapter without losing the current reading path."
					showHeader={false}
					variant="disclosure"
					initialLimit={4}
					queryLimit={8}
				/>
			</MachinesGroupbox>
			<MachinesGroupbox
				legend="Chapter path"
				className="docs-rail-box docs-rail-box--path"
			>
				<DocPathRail breadcrumb={props.breadcrumb} />
			</MachinesGroupbox>
			{props.previous || props.next ? (
				<MachinesGroupbox legend="Chapter navigation" className="docs-rail-box">
					<DocChapterActions previous={props.previous} next={props.next} />
				</MachinesGroupbox>
			) : null}
		</MachinesSidebarPanel>
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
		<div className="site-stack docs-page">
			<SiteHeading
				eyebrow={
					<span className="site-crumbs">
						<a href="/learn">{siteCopy.nav.learn}</a>
						{breadcrumb.map((node, index) => (
							<span key={node.id} className="site-crumbs__node">
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
			<div className="docs-body-grid">
				<MachinesPost className="docs-article">
					<HtmlSnippet className="docs-content" html={page.html} />
					{childPages.length > 0 ? (
						<div className="site-stack site-stack--tight">
							<div className="mx-kicker">{siteCopy.ui.chapters}</div>
							<DocListGroup group={page.title} pages={childPages} />
						</div>
					) : null}
				</MachinesPost>
				<DocPageRail
					breadcrumb={breadcrumb}
					headings={page.headings}
					previous={neighbors.previous}
					next={neighbors.next}
				/>
			</div>
		</div>
	);
}
