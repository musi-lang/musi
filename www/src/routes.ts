import { docsRoutes } from "./docs";
import type { Locale } from "./lib/site-copy";
import { isLocale, siteCopy } from "./lib/site-copy";
import { localizePath } from "./lib/site-links";

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

function localizedPrimaryRoutes(locale: Locale): AppRoute[] {
	const copy = siteCopy[locale];
	return [
		{
			id: `learn:${locale}`,
			path: localizePath(locale, "/learn"),
			label: copy.nav.learn,
			title: `${copy.learn.title} | Musi`,
			description: copy.learn.description,
			kind: "docs-index",
			locale,
			section: "learn",
		},
		{
			id: `install:${locale}`,
			path: localizePath(locale, "/install"),
			label: copy.nav.install,
			title: `${copy.install.title} | Musi`,
			description: copy.install.description,
			kind: "page",
			locale,
			section: "install",
		},
		{
			id: `playground:${locale}`,
			path: localizePath(locale, "/playground"),
			label: copy.nav.playground,
			title: `${copy.playground.title} | Musi`,
			description: copy.playground.copy,
			kind: "page",
			locale,
			section: "playground",
		},
		{
			id: `community:${locale}`,
			path: localizePath(locale, "/community"),
			label: copy.nav.community,
			title: `${copy.community.title} | Musi`,
			description: copy.community.description,
			kind: "page",
			locale,
			section: "community",
		},
	];
}

export const homeRoutes: AppRoute[] = [
	{
		id: "home:en",
		path: "/",
		label: "Musi",
		title: "Musi",
		description: siteCopy.en.home.description,
		kind: "page",
		locale: "en",
		section: "home",
	},
	{
		id: "home:ja",
		path: "/ja",
		label: "Musi",
		title: "Musi",
		description: siteCopy.ja.home.description,
		kind: "page",
		locale: "ja",
		section: "home",
	},
];

export const primaryRoutes: AppRoute[] = [
	...localizedPrimaryRoutes("en"),
	...localizedPrimaryRoutes("ja"),
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

export function localeForPath(pathname: string): Locale {
	const normalized = normalizePath(pathname);
	const firstSegment = normalized.split("/")[1];
	return isLocale(firstSegment) ? firstSegment : "en";
}

export function isDocsRoute(route: AppRoute) {
	return route.kind === "docs-index" || route.kind === "doc";
}
