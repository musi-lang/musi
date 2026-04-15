import { docForPath } from "./docs";
import { SiteLayout } from "./layout/site-layout";
import type { AppRoute } from "./routes";
import { CommunityPage } from "./site-pages/community/page";
import { DocPage, DocsIndexPage } from "./site-pages/docs/page";
import { HomePage } from "./site-pages/home/page";
import { InstallPage } from "./site-pages/install/page";
import { PlaygroundPage } from "./site-pages/playground/page";

function PageBody(props: { route: AppRoute }) {
	if (props.route.kind === "doc" && docForPath(props.route.path)) {
		return <DocPage pathname={props.route.path} route={props.route} />;
	}
	switch (props.route.section) {
		case "learn":
			return <DocsIndexPage route={props.route} />;
		case "install":
			return <InstallPage route={props.route} />;
		case "playground":
			return <PlaygroundPage route={props.route} />;
		case "community":
			return <CommunityPage route={props.route} />;
		default:
			return <HomePage route={props.route} />;
	}
}

export function App(props: { route: AppRoute }) {
	return (
		<SiteLayout route={props.route}>
			<PageBody route={props.route} />
		</SiteLayout>
	);
}
