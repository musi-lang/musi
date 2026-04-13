import { renderedDocs } from "./generated-content";
import type { Locale } from "./lib/site-copy";
import type { AppRoute } from "./routes";

export interface DocHeading {
	depth: number;
	id: string;
	text: string;
}

export interface DocPage {
	id: string;
	kind: "part" | "chapter";
	title: string;
	description: string;
	descriptionHtml: string;
	group: string;
	section: string;
	order: number;
	slug: string;
	summary: string;
	summaryHtml: string;
	headings: DocHeading[];
	html: string;
	path: string;
	canonicalPath: string;
	aliases: string[];
	partId: string;
	partTitle: string;
	questions: { label: string; href: string }[];
	locale: Locale;
}

export interface DocPart {
	id: string;
	title: string;
	summaryHtml: string;
	path: string;
	pages: DocPage[];
	locale: Locale;
}

const localizedDocs = renderedDocs satisfies DocPage[];

const docsByPath = new Map<string, DocPage>();
for (const page of localizedDocs) {
	docsByPath.set(page.path, page);
	for (const alias of page.aliases) {
		docsByPath.set(alias, page);
	}
}

export const docsPages = localizedDocs.filter(
	(page) => page.kind === "chapter",
);
export const docLandingPages = localizedDocs.filter(
	(page) => page.kind === "part",
);

export const docParts = docLandingPages.map((part) => ({
	id: part.id,
	title: part.title,
	summaryHtml: part.summaryHtml,
	path: part.path,
	pages: docsPages.filter(
		(page) => page.partId === part.id && page.locale === part.locale,
	),
	locale: part.locale,
})) satisfies DocPart[];

export const docGroups = docParts.map((part) => ({
	group: part.title,
	path: part.path,
	summaryHtml: part.summaryHtml,
	pages: part.pages,
	locale: part.locale,
}));

export const docQuestionIndex = docsPages.flatMap((page) =>
	page.questions.map((question) => ({
		...question,
		pageTitle: page.title,
		partTitle: page.partTitle,
		locale: page.locale,
	})),
);

export function docForPath(pathname: string) {
	return docsByPath.get(pathname);
}

export function docNeighbors(id: string, locale: Locale) {
	const localizedPages = docsPages.filter((page) => page.locale === locale);
	const index = localizedPages.findIndex(
		(page) => page.id === id || page.slug === id,
	);
	if (index === -1) {
		return {};
	}
	return {
		previous: localizedPages[index - 1],
		next: localizedPages[index + 1],
	};
}

export function pagesForPart(partId: string, locale: Locale) {
	return docsPages.filter(
		(page) => page.partId === partId && page.locale === locale,
	);
}

export function docsRoutes(): AppRoute[] {
	return localizedDocs.flatMap((page) => [
		{
			id: `docs:${page.locale}:${page.id}`,
			label: page.title,
			path: page.path,
			title: `${page.title} | Musi`,
			description: page.description,
			kind: "doc" as const,
			docSlug: page.id,
			canonicalPath: page.canonicalPath,
			locale: page.locale,
			section: "learn" as const,
		},
		...page.aliases.map((alias) => ({
			id: `docs-alias:${page.locale}:${page.id}:${alias}`,
			label: page.title,
			path: alias,
			title: `${page.title} | Musi`,
			description: page.description,
			kind: "doc" as const,
			docSlug: page.id,
			canonicalPath: page.canonicalPath,
			locale: page.locale,
			section: "learn" as const,
		})),
	]);
}
