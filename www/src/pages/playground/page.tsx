import { siteCopy } from "../../lib/site-copy";
import { localizePath } from "../../lib/site-links";
import type { AppRoute } from "../../routes";
import { ActionStrip, InlineAction, SecondaryAction } from "../../ui/actions";
import { Surface } from "../../ui/surface";

export function PlaygroundPage(props: { route: AppRoute }) {
	const copy = siteCopy[props.route.locale].playground;
	return (
		<div className="page-stack">
			<Surface tone="hero" className="section-panel">
				<div className="eyebrow">{copy.eyebrow}</div>
				<h1>{copy.statusTitle}</h1>
				<p className="muted">{copy.statusCopy}</p>
				<ActionStrip>
					<SecondaryAction href={localizePath(props.route.locale, "/install")}>
						{siteCopy[props.route.locale].nav.install}
					</SecondaryAction>
					<InlineAction href={localizePath(props.route.locale, "/community")}>
						{siteCopy[props.route.locale].nav.community}
					</InlineAction>
				</ActionStrip>
			</Surface>
		</div>
	);
}
