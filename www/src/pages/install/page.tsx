import {
	commandRows,
	installCommandGroup,
	installPrerequisites,
	quickstartHtml,
} from "../../content";
import { siteCopy } from "../../lib/site-copy";
import { localizePath } from "../../lib/site-links";
import type { AppRoute } from "../../routes";
import { InlineAction, SecondaryAction } from "../../ui/actions";
import { CodeTabs } from "../../ui/code-tabs";
import { HtmlSnippet } from "../../ui/html-snippet";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";

export function InstallPage(props: { route: AppRoute }) {
	const localeCopy = siteCopy[props.route.locale];
	const copy = localeCopy.install;
	const scriptInstallGroup = installCommandGroup(props.route.locale, "script");
	const cargoInstallGroup = installCommandGroup(props.route.locale, "cargo");
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
					<div className="eyebrow">{copy.installScriptsLabel}</div>
					{scriptInstallGroup ? (
						<div className="code-tabs-stack">
							<p className="muted">{scriptInstallGroup.copy}</p>
							{scriptInstallGroup.tabs ? (
								<CodeTabs
									tabs={scriptInstallGroup.tabs}
									locale={props.route.locale}
									ariaLabel={copy.installScriptsLabel}
								/>
							) : null}
						</div>
					) : null}
				</Surface>
				<Surface tone="code" className="snippet-panel">
					<div className="eyebrow">{copy.cargoInstallLabel}</div>
					{cargoInstallGroup ? (
						<div className="code-tabs-stack">
							<p className="muted">{cargoInstallGroup.copy}</p>
							{cargoInstallGroup.html ? (
								<HtmlSnippet
									className="docs-content"
									html={cargoInstallGroup.html}
									locale={props.route.locale}
								/>
							) : null}
						</div>
					) : null}
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
