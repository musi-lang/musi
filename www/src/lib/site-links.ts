const externalUrlPattern = /^https?:\/\//;
const docsPrefixPattern = /^\/docs/;

import type { Locale } from "./site-copy";
import { localePrefix } from "./site-copy";

export function localizePath(locale: Locale, path: string) {
	if (externalUrlPattern.test(path)) {
		return path;
	}
	if (path === "/learn" || path.startsWith("/learn/")) {
		return path;
	}
	if (locale === "en") {
		if (path === "/") {
			return "/";
		}
		return path;
	}
	if (path === "/") {
		return "/ja";
	}
	return `${localePrefix(locale)}${path}`;
}

export function localizeDocPath(locale: Locale, path: string) {
	const learnPath = path.replace(docsPrefixPattern, "/learn");
	return localizePath(locale, learnPath);
}

export function localizeHtmlLinks(locale: Locale, html: string) {
	return html
		.replaceAll('href="/docs', 'href="/learn')
		.replaceAll('href="/install"', `href="${localizePath(locale, "/install")}"`)
		.replaceAll(
			'href="/community"',
			`href="${localizePath(locale, "/community")}"`,
		)
		.replaceAll(
			'href="/playground"',
			`href="${localizePath(locale, "/playground")}"`,
		);
}
