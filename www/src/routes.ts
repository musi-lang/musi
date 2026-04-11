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
}

export const primaryRoutes: AppRoute[] = [
	{
		id: "home",
		path: "/",
		label: "Home",
		title: "Musi",
		description:
			"Programming language built around first-class resumable effects.",
		kind: "page",
	},
	{
		id: "docs",
		path: "/docs",
		label: "Docs",
		title: "Docs | Musi",
		description: "Guided Musi documentation.",
		kind: "docs-index",
	},
	{
		id: "install",
		path: "/install",
		label: "Install",
		title: "Install | Musi",
		description: "Build Musi from source and start a project.",
		kind: "page",
	},
	{
		id: "reference",
		path: "/reference",
		label: "Reference",
		title: "Reference | Musi",
		description: "Repository, grammar, editor, and project links.",
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
