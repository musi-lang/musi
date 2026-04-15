import type { ReactNode } from "preact/compat";

export interface PageHeaderProps {
	eyebrow: ReactNode;
	title: ReactNode;
	description?: ReactNode;
	descriptionHtml?: string;
	actions?: ReactNode;
	badge?: ReactNode;
	meta?: ReactNode;
	titleAs?: "h1" | "h2";
	titleSize?: "page" | "section";
}

export function PageHeader(props: PageHeaderProps) {
	const TitleTag = props.titleAs ?? "h1";
	const titleClassName = `page-header-title page-header-title-${props.titleSize ?? "page"}`;

	return (
		<div className="page-header">
			<div className="page-header-body">
				<div className="page-header-topline">
					<div className="eyebrow">{props.eyebrow}</div>
					{props.badge ? (
						<div className="page-header-badge">{props.badge}</div>
					) : null}
				</div>
				<TitleTag className={titleClassName}>{props.title}</TitleTag>
				{props.descriptionHtml ? (
					<div
						className="page-copy page-header-copy"
						dangerouslySetInnerHTML={{ __html: props.descriptionHtml }}
					/>
				) : props.description ? (
					<div className="page-copy page-header-copy">{props.description}</div>
				) : null}
				{props.meta ? (
					<div className="page-header-meta">{props.meta}</div>
				) : null}
			</div>
			{props.actions ? (
				<div className="page-header-actions">{props.actions}</div>
			) : null}
		</div>
	);
}
