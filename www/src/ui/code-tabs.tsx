import { useId } from "react";

import type { Locale } from "../lib/site-copy";
import { HtmlSnippet } from "./html-snippet";

export interface CodeTab {
	id: string;
	label: string;
	html: string;
}

export function CodeTabs(props: {
	tabs: readonly CodeTab[];
	locale: Locale;
	ariaLabel: string;
}) {
	const baseId = useId();
	if (props.tabs.length === 0) {
		return null;
	}

	return (
		<div className="code-tabs-shell" data-code-tabs={true}>
			<div
				role="tablist"
				aria-label={props.ariaLabel}
				className="code-tab-list"
			>
				{props.tabs.map((tab) => {
					const tabElementId = `${baseId}-${tab.id}-tab`;
					const panelElementId = `${baseId}-${tab.id}-panel`;
					const selected = tab.id === props.tabs[0]?.id;
					return (
						<button
							key={tab.id}
							type="button"
							id={tabElementId}
							role="tab"
							aria-controls={panelElementId}
							aria-selected={selected}
							tabIndex={selected ? 0 : -1}
							data-code-tab-trigger={tab.id}
							className={
								selected ? "code-tab-trigger is-active" : "code-tab-trigger"
							}
						>
							{tab.label}
						</button>
					);
				})}
			</div>
			{props.tabs.map((tab) => {
				const selected = tab.id === props.tabs[0]?.id;
				return (
					<div
						key={tab.id}
						id={`${baseId}-${tab.id}-panel`}
						role="tabpanel"
						aria-labelledby={`${baseId}-${tab.id}-tab`}
						data-code-tab-panel={tab.id}
						className="code-tab-panel"
						hidden={!selected}
					>
						<HtmlSnippet
							className="docs-content"
							html={tab.html}
							locale={props.locale}
						/>
					</div>
				);
			})}
		</div>
	);
}
