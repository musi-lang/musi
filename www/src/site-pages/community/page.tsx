import { siteCopy } from "../../lib/site-copy";
import type { AppRoute } from "../../routes";
import { InlineAction } from "../../ui/actions";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";

export function CommunityPage(_props: { route: AppRoute }) {
	const copy = siteCopy.community;
	const communityNotes = [
		{
			label: "Source",
			value: "Repository-first project",
			copy: "Code, issues, and contribution notes stay visible rather than hidden behind a signup flow.",
		},
		{
			label: "Support",
			value: "Use public tracking",
			copy: "Open issues are the safest place to follow bugs, language work, and missing docs.",
		},
	];
	return (
		<div className="page-stack">
			<PageHeader
				eyebrow={copy.eyebrow}
				title={copy.title}
				description={copy.description}
			/>
			<Surface tone="well" className="section-panel section-panel-structured">
				<div className="section-heading-row section-heading-bar">
					<div>
						<div className="eyebrow">{copy.eyebrow}</div>
						<h2>{siteCopy.ui.communityLinks}</h2>
					</div>
				</div>
				<section
					className="portal-grid"
					aria-label={siteCopy.ui.communityLinks}
				>
					{copy.sections.map((section) => (
						<Surface
							key={section.title}
							tone="raised"
							className="portal-card portal-card-raised"
						>
							<div className="eyebrow">{section.title}</div>
							<p>{section.copy}</p>
							<InlineAction href={section.href}>{section.label}</InlineAction>
						</Surface>
					))}
				</section>
			</Surface>
			<section className="status-strip" aria-label="Community notes">
				{communityNotes.map((item) => (
					<Surface key={item.label} tone="panel" className="status-card">
						<div className="doc-row-meta">{item.label}</div>
						<strong>{item.value}</strong>
						<span>{item.copy}</span>
					</Surface>
				))}
			</section>
		</div>
	);
}
