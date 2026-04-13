import {
	commandRows,
	installPrerequisites,
	installSourceHtml,
	quickstartHtml,
} from "../../content";
import { siteCopy } from "../../lib/site-copy";
import { localizePath } from "../../lib/site-links";
import type { AppRoute } from "../../routes";
import { InlineAction, SecondaryAction } from "../../ui/actions";
import { HtmlSnippet } from "../../ui/html-snippet";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";

export function InstallPage(props: { route: AppRoute }) {
	const localeCopy = siteCopy[props.route.locale];
	const copy = localeCopy.install;
	return (
		<div className="page-stack">
			<PageHeader
				eyebrow={copy.eyebrow}
				title={copy.title}
				description={copy.description}
				actions={
					<div className="action-strip">
						<SecondaryAction href={localizePath(props.route.locale, "/learn")}>
							{siteCopy[props.route.locale].nav.learn}
						</SecondaryAction>
						<InlineAction href={localizePath(props.route.locale, "/community")}>
							{siteCopy[props.route.locale].nav.community}
						</InlineAction>
					</div>
				}
			/>

			<section className="portal-grid" aria-label={copy.prerequisitesLabel}>
				{installPrerequisites(props.route.locale).map((item) => (
					<Surface key={item.title} tone="panel" className="portal-card">
						<div className="eyebrow">{item.title}</div>
						<h2>{item.value}</h2>
						<p>{item.copy}</p>
					</Surface>
				))}
			</section>

			<section className="code-grid" aria-label={localeCopy.ui.installCommands}>
				<Surface tone="code" className="snippet-panel">
					<div className="eyebrow">{copy.installSourceLabel}</div>
					<HtmlSnippet
						className="docs-content"
						html={installSourceHtml}
						locale={props.route.locale}
					/>
				</Surface>
				<Surface tone="code" className="snippet-panel">
					<div className="eyebrow">{copy.quickStartLabel}</div>
					<HtmlSnippet
						className="docs-content"
						html={quickstartHtml}
						locale={props.route.locale}
					/>
				</Surface>
			</section>

			<Surface tone="panel" className="section-panel">
				<div className="section-heading-row">
					<div>
						<div className="eyebrow">{copy.commandMapLabel}</div>
						<h2>{copy.commandMapTitle}</h2>
						<p className="muted">{copy.commandMapCopy}</p>
					</div>
				</div>
				<div className="table-wrap">
					<table className="data-table">
						<caption className="sr-only">
							{localeCopy.ui.commandReference}
						</caption>
						<thead>
							<tr>
								<th scope="col">{localeCopy.ui.lane}</th>
								<th scope="col">{localeCopy.ui.command}</th>
								<th scope="col">{localeCopy.ui.description}</th>
							</tr>
						</thead>
						<tbody>
							{commandRows(props.route.locale).map((row) => (
								<tr key={row.command}>
									<td>{row.audience}</td>
									<td>
										<code>{row.command}</code>
									</td>
									<td>{row.description}</td>
								</tr>
							))}
						</tbody>
					</table>
				</div>
			</Surface>
		</div>
	);
}
