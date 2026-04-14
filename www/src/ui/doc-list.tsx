import { Surface } from "./surface";

interface DocListPage {
	path: string;
	slug: string;
	summaryHtml: string;
	title: string;
	summary: string;
}

export function DocListGroup(props: {
	group: string;
	path?: string;
	summaryHtml?: string;
	pages: readonly DocListPage[];
	linkLabel?: string;
}) {
	return (
		<Surface className="doc-group" tone="raised">
			<div className="doc-group-header">
				{props.path ? (
					<a href={props.path} className="eyebrow doc-group-link">
						{props.linkLabel
							? `${props.linkLabel}: ${props.group}`
							: props.group}
					</a>
				) : (
					<div className="eyebrow">{props.group}</div>
				)}
				{props.summaryHtml ? (
					<p
						className="muted doc-group-summary"
						dangerouslySetInnerHTML={{ __html: props.summaryHtml }}
					/>
				) : null}
			</div>
			<div className="doc-list">
				{props.pages.map((page) => (
					<a key={page.slug} href={page.path} className="doc-row">
						<div>
							<h3>{page.title}</h3>
							<p dangerouslySetInnerHTML={{ __html: page.summaryHtml }} />
						</div>
						<span className="doc-row-arrow" aria-hidden="true">
							&rarr;
						</span>
					</a>
				))}
			</div>
		</Surface>
	);
}
