import type { DocSearchEntry } from "../docs";

interface DocsSearchProps {
	entries: readonly DocSearchEntry[];
	heading?: string;
	lede?: string;
	className?: string;
	initialLimit?: number;
	queryLimit?: number;
	variant?: "panel" | "disclosure";
}

function entryKicker(entry: DocSearchEntry) {
	const section = entry.sectionTitle ? ` / ${entry.sectionTitle}` : "";
	return `${entry.kind} / ${entry.partTitle}${section}`;
}

export function DocsSearch(props: DocsSearchProps) {
	const initialLimit = props.initialLimit ?? 8;
	const queryLimit = props.queryLimit ?? initialLimit;
	const visibleEntries = props.entries.slice(0, initialLimit);
	const heading = props.heading ?? "Find docs";
	const variant = props.variant ?? "panel";
	const searchBody = (
		<>
			<div className="docs-search-header">
				<div>
					<div className="eyebrow">{heading}</div>
					{props.lede ? <p className="muted">{props.lede}</p> : null}
				</div>
			</div>
			<label className="docs-search-field">
				<span className="sr-only">Search docs</span>
				<input
					type="search"
					placeholder="Search chapters, guides, questions..."
					data-docs-search-input={true}
					aria-label="Search docs"
				/>
			</label>
			<div className="docs-search-results" data-docs-search-results={true}>
				{props.entries.map((entry, index) => (
					<a
						key={entry.id}
						href={entry.path}
						className="docs-search-result"
						data-docs-search-entry={true}
						data-search-text={entry.searchText}
						hidden={index >= visibleEntries.length}
					>
						<span className="doc-row-meta">{entryKicker(entry)}</span>
						<strong>{entry.title}</strong>
						<span>{entry.summary}</span>
					</a>
				))}
			</div>
			<p
				className="muted docs-search-empty"
				data-docs-search-empty={true}
				hidden={true}
			>
				No matching docs yet. Try a command, type, effect, or language name.
			</p>
		</>
	);
	return (
		<search
			className={`docs-search docs-search-${variant}${props.className ? ` ${props.className}` : ""}`}
			data-docs-search={true}
			data-docs-search-initial-limit={initialLimit}
			data-docs-search-query-limit={queryLimit}
		>
			{variant === "disclosure" ? (
				<details
					className="docs-search-disclosure"
					data-docs-search-details={true}
				>
					<summary className="docs-search-summary">
						<span className="eyebrow">{heading}</span>
						<span className="docs-search-summary-cue" aria-hidden={true}>
							⌄
						</span>
					</summary>
					<div className="docs-search-drawer">{searchBody}</div>
				</details>
			) : (
				searchBody
			)}
		</search>
	);
}
