import { docForPath } from "./docs";
import { SiteLayout } from "./layout/site-layout";
import { useRouteState } from "./navigation";
import { CommunityPage } from "./pages/community/page";
import { DocPage, DocsIndexPage } from "./pages/docs/page";
import { HomePage } from "./pages/home/page";
import { InstallPage } from "./pages/install/page";
import { PlaygroundPage } from "./pages/playground/page";
import type { AppRoute } from "./routes";
import { ThemeProvider } from "./ui/theme";

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
	const routeState = useRouteState(props.route);
	return (
		<ThemeProvider>
			<SiteLayout route={routeState.route}>
				<PageBody route={routeState.route} />
			</SiteLayout>
		</ThemeProvider>
	);
}
