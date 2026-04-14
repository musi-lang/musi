import { siteCopy } from "../../lib/site-copy";
import type { AppRoute } from "../../routes";
import { InlineAction } from "../../ui/actions";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";

export function CommunityPage(props: { route: AppRoute }) {
	const copy = siteCopy[props.route.locale].community;
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
						<h2>{siteCopy[props.route.locale].ui.communityLinks}</h2>
					</div>
				</div>
				<section
					className="portal-grid portal-grid-compact"
					aria-label={siteCopy[props.route.locale].ui.communityLinks}
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
		</div>
	);
}
