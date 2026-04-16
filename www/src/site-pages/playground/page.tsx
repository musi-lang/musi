import { siteCopy } from "../../lib/site-copy";
import type { AppRoute } from "../../routes";
import { ActionStrip, InlineAction, SecondaryAction } from "../../ui/actions";
import { Surface } from "../../ui/surface";

export function PlaygroundPage(_props: { route: AppRoute }) {
	const copy = siteCopy.playground;
	const statusCards = [
		{
			label: "Current",
			title: "Browser runner not public",
			copy: "Use the source install path for real checks and tests today.",
			href: "/install",
			action: "Install locally",
		},
		{
			label: "Fallback",
			title: "Read runnable examples",
			copy: "The book keeps examples close to supported syntax and commands.",
			href: "/learn/book",
			action: "Open book",
		},
		{
			label: "Track work",
			title: "Follow implementation status",
			copy: "Issues and repository links show current playground and runtime work.",
			href: "/community",
			action: "Open community",
		},
	];
	return (
		<div className="page-stack">
			<Surface tone="hero" className="section-panel portal-status-panel">
				<div className="eyebrow">{copy.eyebrow}</div>
				<h1>{copy.statusTitle}</h1>
				<p className="muted">{copy.statusCopy}</p>
				<ActionStrip>
					<SecondaryAction href="/install">
						{siteCopy.nav.install}
					</SecondaryAction>
					<InlineAction href="/community">
						{siteCopy.nav.community}
					</InlineAction>
				</ActionStrip>
			</Surface>
			<section className="portal-grid" aria-label="Playground paths">
				{statusCards.map((card) => (
					<Surface
						key={card.title}
						tone="raised"
						className="portal-card portal-card-raised portal-card-task"
					>
						<div className="eyebrow">{card.label}</div>
						<h2>{card.title}</h2>
						<p>{card.copy}</p>
						<InlineAction href={card.href}>{card.action}</InlineAction>
					</Surface>
				))}
			</section>
		</div>
	);
}
