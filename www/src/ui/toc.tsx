import { Surface } from "./surface";

export function OnThisPage(props: {
	headings: Array<{ depth: number; id: string; text: string }>;
	className?: string;
	label?: string;
}) {
	const headings = props.headings.filter((heading) => heading.depth <= 3);
	if (headings.length === 0) {
		return null;
	}

	return (
		<Surface
			className={`toc-panel${props.className ? ` ${props.className}` : ""}`}
			tone="panel"
		>
			<div className="eyebrow">{props.label ?? "On this page"}</div>
			<nav aria-label={props.label ?? "On this page"} className="toc-nav">
				{headings.map((heading) => (
					<a
						key={heading.id}
						href={`#${heading.id}`}
						className={`toc-link${heading.depth === 3 ? " toc-link-child" : ""}`}
					>
						{heading.text}
					</a>
				))}
			</nav>
		</Surface>
	);
}
