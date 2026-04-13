import { docForPath, docsRoutes } from "./docs";

export type AppRouteKind = "page" | "docs-index" | "doc";

export interface AppRoute {
	id: string;
	path: string;
	label: string;
	title: string;
	description: string;
	kind: AppRouteKind;
	docSlug?: string;
	canonicalPath?: string;
	disabled?: boolean;
}

export const primaryRoutes: AppRoute[] = [
	{
		id: "home",
		path: "/",
		label: "Home",
		title: "Musi",
		description:
			"Musi language overview, docs, install steps, and public references.",
		kind: "page",
	},
	{
		id: "playground",
		path: "/playground",
		label: "Playground",
		title: "Playground | Musi",
		description: "Musi WASM web compiler",
		kind: "page",
		disabled: true,
	},
	{
		id: "docs",
		path: "/docs",
		label: "Docs",
		title: "Docs | Musi",
		description:
			"Musi docs with current syntax, stdlib names, and learning order.",
		kind: "docs-index",
	},
	{
		id: "install",
		path: "/install",
		label: "Install",
		title: "Install | Musi",
		description:
			"Build Musi from source and set up direct and package commands.",
		kind: "page",
	},
	{
		id: "reference",
		path: "/reference",
		label: "Reference",
		title: "Reference | Musi",
		description:
			"Repository, grammar, editor, and public source links for Musi.",
		kind: "page",
	},
];

export const appRoutes: AppRoute[] = [...primaryRoutes, ...docsRoutes()];

export function normalizePath(pathname: string) {
	if (pathname === "") {
		return "/";
	}
	if (pathname.length > 1 && pathname.endsWith("/")) {
		return pathname.slice(0, -1);
	}
	return pathname;
}

export function routeForPath(pathname: string) {
	const normalized = normalizePath(pathname);
	return (
		appRoutes.find((route) => route.path === normalized) ?? primaryRoutes[0]
	);
}

export function isDocsRoute(route: AppRoute) {
	return route.kind === "docs-index" || route.kind === "doc";
}

export function routeDocument(route: AppRoute) {
	if (route.kind !== "doc") {
		return undefined;
	}
	return docForPath(route.path);
}
