import { useId } from "preact/compat";

import { HtmlSnippet } from "./html-snippet";

export interface CodeTab {
	id: string;
	label: string;
	html: string;
}

export function CodeTabs(props: {
	tabs: readonly CodeTab[];
	ariaLabel: string;
}) {
	const baseId = useId();
	if (props.tabs.length === 0) {
		return null;
	}

	return (
		<div className="mx-code-tabs" data-code-tabs={true}>
			<div
				className="mx-nav-rail mx-code-tabs__rail"
				role="tablist"
				aria-label={props.ariaLabel}
			>
				<ul className="mx-nav-tabs">
					{props.tabs.map((tab) => {
						const tabElementId = `${baseId}-${tab.id}-tab`;
						const panelElementId = `${baseId}-${tab.id}-panel`;
						const selected = tab.id === props.tabs[0]?.id;
						return (
							<li key={tab.id}>
								<button
									type="button"
									id={tabElementId}
									role="tab"
									aria-controls={panelElementId}
									aria-selected={selected}
									tabIndex={selected ? 0 : -1}
									data-code-tab-trigger={tab.id}
									className={selected ? "mx-tab is-active" : "mx-tab"}
								>
									{tab.label}
								</button>
							</li>
						);
					})}
				</ul>
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
						className="mx-code-frame mx-code-tabs__frame"
						hidden={!selected}
					>
						<HtmlSnippet html={tab.html} />
					</div>
				);
			})}
		</div>
	);
}
