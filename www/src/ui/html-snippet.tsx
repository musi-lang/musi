export function HtmlSnippet(props: { className?: string; html: string }) {
	return (
		<div
			className={props.className}
			dangerouslySetInnerHTML={{ __html: props.html }}
		/>
	);
}
