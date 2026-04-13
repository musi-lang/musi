import { createRoot, hydrateRoot } from "react-dom/client";
import { App } from "./pages";
import "./app.css";
import { routeForPath } from "./routes";

const root = document.getElementById("root");

if (!root) {
	throw new Error("missing root container");
}

const route = routeForPath(window.location.pathname);
const app = <App route={route} />;

if (root.hasChildNodes()) {
	hydrateRoot(root, app);
} else {
	createRoot(root).render(app);
}
