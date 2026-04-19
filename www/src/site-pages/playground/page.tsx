import { siteCopy } from "../../lib/site-copy";
import type { AppRoute } from "../../routes";
import { ActionStrip, InlineAction, SecondaryAction } from "../../ui/actions";
import { MachinesActionCard, MachinesPanel } from "../../ui/machines";

export function PlaygroundPage(_props: { route: AppRoute }) {
	const copy = siteCopy.playground;
	const statusCards = [
		{
			label: "Local install",
			title: "Use local runner",
			copy: "Use source install for checks and tests.",
			href: "/install",
			action: "Install",
		},
		{
			label: "Examples",
			title: "Read examples",
			copy: "Open runnable examples and commands.",
			href: "/learn/book",
			action: "Open book",
		},
		{
			label: "Status",
			title: "Track work",
			copy: "Follow playground and runtime work in public issues.",
			href: "/community",
			action: "Community",
		},
	];
	return (
		<div className="site-stack">
			<MachinesPanel title={copy.eyebrow}>
				<div className="site-heading__body">
					<div>
						<h1 className="site-title site-title--page">{copy.statusTitle}</h1>
						<p className="site-copy">{copy.statusCopy}</p>
					</div>
					<ActionStrip>
						<SecondaryAction href="/install">
							{siteCopy.nav.install}
						</SecondaryAction>
						<InlineAction href="/community">
							{siteCopy.nav.community}
						</InlineAction>
					</ActionStrip>
				</div>
			</MachinesPanel>
			<section
				className="mx-grid site-grid--three"
				aria-label="Playground paths"
			>
				{statusCards.map((card) => (
					<MachinesActionCard
						key={card.title}
						href={card.href}
						kicker={card.label}
						title={card.title}
						description={card.copy}
						action={card.action}
					/>
				))}
			</section>
		</div>
	);
}
