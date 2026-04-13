import { renderedDocs } from "./generated-content";
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
}

export interface DocPart {
	id: string;
	title: string;
	summaryHtml: string;
	path: string;
	pages: DocPage[];
}

const canonicalDocs = renderedDocs.map((document) => {
	const partTitle =
		document.kind === "part"
			? document.title
			: (renderedDocs.find(
					(candidate) =>
						candidate.kind === "part" && candidate.id === document.partId,
				)?.title ?? document.partTitle);

	return {
		...document,
		partTitle,
	} satisfies DocPage;
});

const docsByPath = new Map<string, DocPage>();
for (const page of canonicalDocs) {
	docsByPath.set(page.path, page);
	for (const alias of page.aliases) {
		docsByPath.set(alias, page);
	}
}

export const docsPages = canonicalDocs.filter(
	(page) => page.kind === "chapter",
);
export const docLandingPages = canonicalDocs.filter(
	(page) => page.kind === "part",
);

export const docParts = docLandingPages.map((part) => ({
	id: part.id,
	title: part.title,
	summaryHtml: part.summaryHtml,
	path: part.path,
	pages: docsPages.filter((page) => page.partId === part.id),
})) satisfies DocPart[];

export const docGroups = docParts.map((part) => ({
	group: part.title,
	path: part.path,
	summaryHtml: part.summaryHtml,
	pages: part.pages,
}));

export const docQuestionIndex = docsPages.flatMap((page) =>
	page.questions.map((question) => ({
		...question,
		pageTitle: page.title,
		partTitle: page.partTitle,
	})),
);

export function docForPath(pathname: string) {
	return docsByPath.get(pathname);
}

export function docNeighbors(id: string) {
	const index = docsPages.findIndex(
		(page) => page.id === id || page.slug === id,
	);
	if (index === -1) {
		return {};
	}

	return {
		previous: docsPages[index - 1],
		next: docsPages[index + 1],
	};
}

export function pagesForPart(partId: string) {
	return docsPages.filter((page) => page.partId === partId);
}

export function docsRoutes(): AppRoute[] {
	return canonicalDocs.flatMap((page) => [
		{
			id: `docs:${page.id}`,
			label: page.title,
			path: page.path,
			title: `${page.title} | Musi`,
			description: page.description,
			kind: "doc",
			docSlug: page.id,
			canonicalPath: page.canonicalPath,
		},
		...page.aliases.map((alias) => ({
			id: `docs-alias:${page.id}:${alias}`,
			label: page.title,
			path: alias,
			title: `${page.title} | Musi`,
			description: page.description,
			kind: "doc" as const,
			docSlug: page.id,
			canonicalPath: page.canonicalPath,
		})),
	]);
}
