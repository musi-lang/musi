import { renderToString } from "react-dom/server";
import { App } from "./pages";
import type { AppRoute } from "./routes";

export function renderRoute(route: AppRoute) {
	return renderToString(<App route={route} />);
}
