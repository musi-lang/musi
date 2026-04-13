import { useId, useState } from "react";

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
	const [activeTabId, setActiveTabId] = useState(props.tabs[0]?.id ?? "");
	const activeTab =
		props.tabs.find((tab) => tab.id === activeTabId) ?? props.tabs[0];
	if (!activeTab) {
		return null;
	}

	return (
		<div className="code-tabs-shell">
			<div
				role="tablist"
				aria-label={props.ariaLabel}
				className="code-tab-list"
			>
				{props.tabs.map((tab) => {
					const tabElementId = `${baseId}-${tab.id}-tab`;
					const panelElementId = `${baseId}-${tab.id}-panel`;
					const selected = tab.id === activeTab.id;
					return (
						<button
							key={tab.id}
							type="button"
							id={tabElementId}
							role="tab"
							aria-controls={panelElementId}
							aria-selected={selected}
							className={
								selected ? "code-tab-trigger is-active" : "code-tab-trigger"
							}
							onClick={() => setActiveTabId(tab.id)}
						>
							{tab.label}
						</button>
					);
				})}
			</div>
			<div
				id={`${baseId}-${activeTab.id}-panel`}
				role="tabpanel"
				aria-labelledby={`${baseId}-${activeTab.id}-tab`}
				className="code-tab-panel"
			>
				<HtmlSnippet
					className="docs-content"
					html={activeTab.html}
					locale={props.locale}
				/>
			</div>
		</div>
	);
}
