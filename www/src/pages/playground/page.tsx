import { siteCopy } from "../../lib/site-copy";
import type { AppRoute } from "../../routes";
import { ActionStrip, InlineAction, SecondaryAction } from "../../ui/actions";
import { Surface } from "../../ui/surface";

export function PlaygroundPage(_props: { route: AppRoute }) {
	const copy = siteCopy.playground;
	return (
		<div className="page-stack">
			<Surface tone="hero" className="section-panel">
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
		</div>
	);
}
