import {
	docForPath,
	docGroups,
	docNeighbors,
	docQuestionIndex,
	pagesForPart,
} from "../../docs";
import { siteCopy } from "../../lib/site-copy";
import { localizePath } from "../../lib/site-links";
import type { AppRoute } from "../../routes";
import { InlineAction, PrimaryAction, SecondaryAction } from "../../ui/actions";
import { DocListGroup } from "../../ui/doc-list";
import { HtmlSnippet } from "../../ui/html-snippet";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";
import { OnThisPage } from "../../ui/toc";

const PART_REGEXP = /\/[^/]+$/;

export function DocsIndexPage(props: { route: AppRoute }) {
	const localeCopy = siteCopy[props.route.locale];
	const copy = localeCopy.learn;
	return (
		<div className="page-stack">
			<PageHeader
				eyebrow={copy.eyebrow}
				title={copy.title}
				description={copy.description}
				actions={
					<div className="action-strip">
						<PrimaryAction href={props.route.path}>
							{siteCopy[props.route.locale].nav.learn}
						</PrimaryAction>
						<SecondaryAction
							href={localizePath(props.route.locale, "/install")}
						>
							{siteCopy[props.route.locale].nav.install}
						</SecondaryAction>
						<InlineAction href={localizePath(props.route.locale, "/community")}>
							{siteCopy[props.route.locale].nav.community}
						</InlineAction>
					</div>
				}
			/>
			<section
				className="portal-grid"
				aria-label={localeCopy.ui.docsEntryPoints}
			>
				<Surface tone="accent" className="portal-card">
					<div className="eyebrow">{copy.eyebrow}</div>
					<h2>{copy.startTitle}</h2>
					<p>{copy.description}</p>
					<InlineAction
						href={localizePath(
							props.route.locale,
							"/learn/start/getting-started",
						)}
					>
						{localeCopy.ui.openFirstChapter}
					</InlineAction>
				</Surface>
				<Surface tone="panel" className="portal-card">
					<div className="eyebrow">{localeCopy.ui.questions}</div>
					<h2>{copy.questionsTitle}</h2>
					<p>
						{props.route.locale === "ja"
							? "やりたい作業は分かっていても、どの章を見るべきか分からないときに使います。"
							: "Use the task-first index when you know the job but not the chapter."}
					</p>
					<InlineAction
						href={localizePath(
							props.route.locale,
							"/learn/questions/common-questions",
						)}
					>
						{localeCopy.ui.openQuestions}
					</InlineAction>
				</Surface>
			</section>
			<Surface tone="panel" className="section-panel">
				<div className="section-heading-row">
					<div>
						<div className="eyebrow">{localeCopy.ui.learnSection}</div>
						<h2>{copy.partsTitle}</h2>
					</div>
				</div>
				<div className="doc-groups-grid doc-groups-grid-compact">
					{docGroups
						.filter((group) => group.locale === props.route.locale)
						.map((group) => (
							<DocListGroup
								key={`${group.locale}:${group.group}`}
								group={group.group}
								path={group.path}
								summaryHtml={group.summaryHtml}
								pages={group.pages}
							/>
						))}
				</div>
			</Surface>
			<Surface tone="panel" className="section-panel">
				<div className="section-heading-row">
					<div>
						<div className="eyebrow">{localeCopy.ui.questions}</div>
						<h2>{copy.questionsTitle}</h2>
					</div>
				</div>
				<div className="question-grid">
					{docQuestionIndex
						.filter((question) => question.locale === props.route.locale)
						.map((question) => (
							<a
								key={`${question.href}:${question.label}`}
								href={question.href}
								className="question-card"
							>
								<strong dangerouslySetInnerHTML={{ __html: question.label }} />
								<span>{question.pageTitle}</span>
							</a>
						))}
				</div>
			</Surface>
		</div>
	);
}

function SectionNav(props: {
	locale: "en" | "ja";
	previous: { path: string; title: string } | undefined;
	next: { path: string; title: string } | undefined;
}) {
	if (!(props.previous || props.next)) {
		return null;
	}
	const labels = siteCopy[props.locale].ui;
	return (
		<nav className="section-nav" aria-label={labels.chapterNavigation}>
			<div>
				{props.previous ? (
					<InlineAction href={props.previous.path}>
						{labels.previousChapter}: {props.previous.title}
					</InlineAction>
				) : null}
			</div>
			<div>
				{props.next ? (
					<InlineAction href={props.next.path}>
						{labels.nextChapter}: {props.next.title}
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
	const neighbors =
		page.kind === "chapter" ? docNeighbors(page.id, page.locale) : {};
	const childPages =
		page.kind === "part" ? pagesForPart(page.id, page.locale) : [];
	return (
		<div className="page-stack docs-page">
			<PageHeader
				eyebrow={
					<span className="crumbs">
						<a href={localizePath(page.locale, "/learn")}>
							{siteCopy[page.locale].nav.learn}
						</a>
						<span aria-hidden="true">/</span>
						{page.kind === "part" ? (
							<span>{page.title}</span>
						) : (
							<a href={page.canonicalPath.replace(PART_REGEXP, "")}>
								{page.partTitle}
							</a>
						)}
					</span>
				}
				title={page.title}
				descriptionHtml={page.descriptionHtml}
			/>
			<OnThisPage
				headings={page.headings}
				className="toc-panel-mobile"
				label={siteCopy[page.locale].ui.onThisPage}
			/>
			<div className="docs-body-grid">
				<Surface tone="base" className="doc-article-surface">
					<article className="docs-article">
						<HtmlSnippet
							className="docs-content"
							html={page.html}
							locale={page.locale}
						/>
						{page.kind === "part" && childPages.length > 0 ? (
							<div className="part-children-block">
								<div className="eyebrow">
									{siteCopy[page.locale].ui.chapters}
								</div>
								<DocListGroup group={page.title} pages={childPages} />
							</div>
						) : null}
					</article>
				</Surface>
				<OnThisPage
					headings={page.headings}
					className="toc-panel-desktop"
					label={siteCopy[page.locale].ui.onThisPage}
				/>
			</div>
			{page.kind === "chapter" ? (
				<SectionNav
					locale={page.locale}
					previous={neighbors.previous}
					next={neighbors.next}
				/>
			) : null}
		</div>
	);
}
