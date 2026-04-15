import { docsRoutes } from "./docs";
import type { Locale } from "./lib/site-copy";
import { siteCopy } from "./lib/site-copy";

export type AppRouteKind = "page" | "docs-index" | "doc";

export type AppSection =
	| "home"
	| "learn"
	| "install"
	| "playground"
	| "community";

export interface AppRoute {
	id: string;
	path: string;
	label: string;
	title: string;
	description: string;
	kind: AppRouteKind;
	locale: Locale;
	section: AppSection;
	docSlug?: string;
	canonicalPath?: string;
	disabled?: boolean;
}

export const homeRoutes: AppRoute[] = [
	{
		id: "home:en",
		path: "/",
		label: "Musi",
		title: "Musi",
		description: siteCopy.home.description,
		kind: "page",
		locale: "en",
		section: "home",
	},
];

export const primaryRoutes: AppRoute[] = [
	{
		id: "learn:en",
		path: "/learn",
		label: siteCopy.nav.learn,
		title: `${siteCopy.learn.title} | Musi`,
		description: siteCopy.learn.description,
		kind: "docs-index",
		locale: "en",
		section: "learn",
	},
	{
		id: "install:en",
		path: "/install",
		label: siteCopy.nav.install,
		title: `${siteCopy.install.title} | Musi`,
		description: siteCopy.install.description,
		kind: "page",
		locale: "en",
		section: "install",
	},
	{
		id: "playground:en",
		path: "/playground",
		label: siteCopy.nav.playground,
		title: `${siteCopy.playground.title} | Musi`,
		description: siteCopy.playground.copy,
		kind: "page",
		locale: "en",
		section: "playground",
	},
	{
		id: "community:en",
		path: "/community",
		label: siteCopy.nav.community,
		title: `${siteCopy.community.title} | Musi`,
		description: siteCopy.community.description,
		kind: "page",
		locale: "en",
		section: "community",
	},
];

export const appRoutes: AppRoute[] = [
	...homeRoutes,
	...primaryRoutes,
	...docsRoutes(),
];

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
	return appRoutes.find((route) => route.path === normalized) ?? homeRoutes[0];
}

export function localeForPath(_pathname: string): Locale {
	return "en";
}

export function isDocsRoute(route: AppRoute) {
	return route.kind === "docs-index" || route.kind === "doc";
}
