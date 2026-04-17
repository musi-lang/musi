import {
	commandRows,
	installCommandGroup,
	installPrerequisites,
	quickstartHtml,
} from "../../content";
import { siteCopy } from "../../lib/site-copy";
import type { AppRoute } from "../../routes";
import { InlineAction, SecondaryAction } from "../../ui/actions";
import { CodeTabs } from "../../ui/code-tabs";
import { HtmlSnippet } from "../../ui/html-snippet";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";

export function InstallPage(_props: { route: AppRoute }) {
	const copy = siteCopy.install;
	const scriptInstallGroup = installCommandGroup("script");
	const cargoInstallGroup = installCommandGroup("cargo");
	const installSteps = [
		{
			label: "1",
			title: "Check prerequisites",
			copy: "Rust, Cargo, and native library pieces come first.",
		},
		{
			label: "2",
			title: "Choose command lane",
			copy: "Script bootstrap is quickest; Cargo path keeps source visible.",
		},
		{
			label: "3",
			title: "Verify PATH",
			copy: "Both music and musi land in Cargo's bin directory.",
		},
		{
			label: "4",
			title: "Use current commands",
			copy: "Package commands cover normal work; direct commands handle files.",
		},
	];
	return (
		<div className="page-stack">
			<PageHeader
				eyebrow={copy.eyebrow}
				title={copy.title}
				description={copy.description}
				actions={
					<div className="action-strip">
						<SecondaryAction href="/learn">
							{siteCopy.nav.learn}
						</SecondaryAction>
						<InlineAction href="/community">
							{siteCopy.nav.community}
						</InlineAction>
					</div>
				}
			/>

			<section className="process-grid" aria-label="Install process">
				{installSteps.map((step) => (
					<Surface key={step.label} tone="raised" className="process-card">
						<div className="process-step">{step.label}</div>
						<div>
							<h2>{step.title}</h2>
							<p>{step.copy}</p>
						</div>
					</Surface>
				))}
			</section>

			<section className="portal-grid" aria-label={copy.prerequisitesLabel}>
				{installPrerequisites().map((item) => (
					<Surface
						key={item.title}
						tone="panel"
						className="portal-card portal-card-task"
					>
						<div className="eyebrow">{item.title}</div>
						<h2>{item.value}</h2>
						<p>{item.copy}</p>
					</Surface>
				))}
			</section>

			<section className="code-grid" aria-label={siteCopy.ui.installCommands}>
				<Surface tone="code" className="snippet-panel">
					<div className="eyebrow">{copy.installScriptsLabel}</div>
					{scriptInstallGroup ? (
						<div className="code-tabs-stack">
							<p className="muted">{scriptInstallGroup.copy}</p>
							{scriptInstallGroup.tabs ? (
								<CodeTabs
									tabs={scriptInstallGroup.tabs}
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
								/>
							) : null}
						</div>
					) : null}
				</Surface>
				<Surface tone="code" className="snippet-panel">
					<div className="eyebrow">{copy.quickStartLabel}</div>
					<HtmlSnippet className="docs-content" html={quickstartHtml} />
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
							{siteCopy.ui.commandReference}
						</caption>
						<thead>
							<tr>
								<th scope="col">{siteCopy.ui.lane}</th>
								<th scope="col">{siteCopy.ui.command}</th>
								<th scope="col">{siteCopy.ui.description}</th>
							</tr>
						</thead>
						<tbody>
							{commandRows().map((row) => (
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
