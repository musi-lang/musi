import { useLayoutEffect, useRef } from "react";
import { initCodeTabsWithin } from "../code-tabs";

export function HtmlSnippet(props: { className?: string; html: string }) {
	const rootRef = useRef<HTMLDivElement | null>(null);

	useLayoutEffect(() => {
		if (!rootRef.current) {
			return;
		}
		initCodeTabsWithin(rootRef.current);
	});

	return (
		<div
			ref={rootRef}
			className={props.className}
			dangerouslySetInnerHTML={{ __html: props.html }}
		/>
	);
}
