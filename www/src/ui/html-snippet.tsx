import type { Locale } from "../lib/site-copy";
import { localizeHtmlLinks } from "../lib/site-links";

export function HtmlSnippet(props: {
	className?: string;
	html: string;
	locale: Locale;
}) {
	return (
		<div
			className={props.className}
			dangerouslySetInnerHTML={{
				__html: localizeHtmlLinks(props.locale, props.html),
			}}
		/>
	);
}
