import {
	ColorSchemeScript,
	localStorageColorSchemeManager,
	MantineProvider,
} from "@mantine/core";
import { docForPath } from "./docs";
import { PageFooter, SiteLayout } from "./layout/site-layout";
import { useRouteState } from "./navigation";
import { DocPage, DocsIndexPage } from "./pages/docs/page";
import { HomePage } from "./pages/home/page";
import { InstallPage } from "./pages/install/page";
import { ReferencePage } from "./pages/reference/page";
import type { AppRoute } from "./routes";
import { theme } from "./theme";

const colorSchemeManager = localStorageColorSchemeManager({
	key: "musi-color-scheme",
});

function PageBody(props: { route: AppRoute }) {
	if (props.route.kind === "doc" && docForPath(props.route.path)) {
		return <DocPage pathname={props.route.path} />;
	}

	switch (props.route.id) {
		case "docs":
			return <DocsIndexPage />;
		case "install":
			return <InstallPage />;
		case "reference":
			return <ReferencePage />;
		default:
			return <HomePage />;
	}
}

export function App(props: { route: AppRoute }) {
	const routeState = useRouteState(props.route);

	return (
		<>
			<ColorSchemeScript
				defaultColorScheme="auto"
				localStorageKey="musi-color-scheme"
			/>
			<MantineProvider
				theme={theme}
				defaultColorScheme="auto"
				colorSchemeManager={colorSchemeManager}
			>
				<SiteLayout route={routeState.route}>
					<PageBody route={routeState.route} />
					<PageFooter />
				</SiteLayout>
			</MantineProvider>
		</>
	);
}
