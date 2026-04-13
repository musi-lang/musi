import "@mantine/core/styles.css";
import { MantineProvider } from "@mantine/core";
import { createRoot, hydrateRoot } from "react-dom/client";
import { App } from "./pages";
import { theme } from "./theme";
import "./app.css";
import { routeForPath } from "./routes";

const root = document.getElementById("root");

if (!root) {
	throw new Error("missing root container");
}

const route = routeForPath(window.location.pathname);
const app = (
	<MantineProvider theme={theme} defaultColorScheme="auto">
		<App route={route} />
	</MantineProvider>
);

if (root.hasChildNodes()) {
	hydrateRoot(root, app);
} else {
	createRoot(root).render(app);
}
