import { renderedDocs } from "./generated-content";
import type { AppRoute } from "./routes";

export interface DocHeading {
	depth: number;
	id: string;
	text: string;
}

export interface DocPage {
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
}

export const docsPages = renderedDocs
	.map((document) => ({
		...document,
		path: `/docs/${document.slug}`,
	}))
	.sort((left, right) => left.order - right.order);

const groupedPages = new Map<string, DocPage[]>();

for (const page of docsPages) {
	const pages = groupedPages.get(page.group);
	if (pages) {
		pages.push(page);
		continue;
	}

	groupedPages.set(page.group, [page]);
}

export const docGroups = Array.from(groupedPages, ([group, pages]) => ({
	group,
	pages,
}));

export function docForPath(pathname: string) {
	return docsPages.find((page) => page.path === pathname);
}

export function docNeighbors(slug: string) {
	const index = docsPages.findIndex((page) => page.slug === slug);
	if (index === -1) {
		return {};
	}

	return {
		previous: docsPages[index - 1],
		next: docsPages[index + 1],
	};
}

export function docsRoutes(): AppRoute[] {
	return docsPages.map((page) => ({
		id: `docs:${page.slug}`,
		label: page.title,
		path: page.path,
		title: `${page.title} | Musi`,
		description: page.description,
		kind: "doc",
		docSlug: page.slug,
	}));
}
