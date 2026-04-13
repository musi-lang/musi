import { homeSampleHtml } from "../../content";
import { docGroups } from "../../docs";
import { siteCopy } from "../../lib/site-copy";
import { localizePath } from "../../lib/site-links";
import type { AppRoute } from "../../routes";
import {
	ActionStrip,
	InlineAction,
	PrimaryAction,
	SecondaryAction,
} from "../../ui/actions";
import { DocListGroup } from "../../ui/doc-list";
import { HtmlSnippet } from "../../ui/html-snippet";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";

export function HomePage(props: { route: AppRoute }) {
	const localeCopy = siteCopy[props.route.locale];
	const copy = localeCopy.home;
	return (
		<div className="page-stack page-home">
			<Surface tone="hero" className="hero-grid hero-shell">
				<div className="hero-copy-column">
					<PageHeader
						eyebrow={copy.eyebrow}
						title={copy.title}
						description={copy.description}
						meta={
							<ul className="hero-summary-list">
								{copy.summary.map((item) => (
									<li key={item} className="hero-summary-item">
										{item}
									</li>
								))}
							</ul>
						}
						actions={
							<ActionStrip>
								<PrimaryAction
									href={localizePath(props.route.locale, "/learn")}
								>
									{copy.primaryCta}
								</PrimaryAction>
								<SecondaryAction
									href={localizePath(props.route.locale, "/install")}
								>
									{copy.secondaryCta}
								</SecondaryAction>
								<InlineAction
									href={localizePath(props.route.locale, "/community")}
								>
									{copy.tertiaryCta}
								</InlineAction>
							</ActionStrip>
						}
					/>
				</div>
				<Surface
					tone="raised"
					className="snippet-panel hero-code-panel hero-float-panel"
				>
					<div className="eyebrow">{localeCopy.ui.sample}</div>
					<HtmlSnippet
						className="docs-content"
						html={homeSampleHtml}
						locale={props.route.locale}
					/>
				</Surface>
			</Surface>

			<section className="portal-grid" aria-label={localeCopy.ui.primaryPaths}>
				{copy.paths.map((link) => (
					<Surface
						key={link.title}
						tone="raised"
						className="portal-card portal-card-raised"
					>
						<div className="eyebrow">{link.label}</div>
						<h2>{link.title}</h2>
						<p>{link.copy}</p>
						<InlineAction href={link.href}>{localeCopy.ui.open}</InlineAction>
					</Surface>
				))}
			</section>

			<Surface
				tone="well"
				className="section-panel section-panel-structured why-panel"
			>
				<div className="section-heading-row section-heading-bar">
					<div>
						<div className="eyebrow">{localeCopy.ui.learnSection}</div>
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
						<div className="eyebrow">{localeCopy.ui.learnSection}</div>
						<h2>{localeCopy.learn.partsTitle}</h2>
					</div>
					<InlineAction href={localizePath(props.route.locale, "/learn")}>
						{localeCopy.nav.learn}
					</InlineAction>
				</div>
				<div className="doc-groups-grid doc-groups-grid-separated">
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
		</div>
	);
}
