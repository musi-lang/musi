import { bookPages, bookParts, bookSections } from "./book/manifest";
import type { BookSectionDefinition } from "./book/registry/types";

export type ContentCollectionId = "book" | "guides" | "reference";
export type ContentAudience = "beginner" | "language-transition" | "reference";

export interface ContentCollection {
	id: ContentCollectionId;
	title: string;
	path: string;
	audience: ContentAudience;
}

export interface ContentEntry {
	id: string;
	collection: ContentCollectionId;
	path: string;
	aliases: readonly string[];
	sourcePath: string;
	partId: string;
	sectionId?: string;
}

function collectionForPart(partId: string): ContentCollectionId {
	return partId === "developers" ? "guides" : "book";
}

function sectionSourcePath(section: BookSectionDefinition) {
	return section.sourcePath ?? `generated://book-sections/${section.id}.md`;
}

export const contentCollections = [
	{
		id: "book",
		title: "Musi Book",
		path: "/learn/book",
		audience: "beginner",
	},
	{
		id: "guides",
		title: "Musi for Developers",
		path: "/learn/guides",
		audience: "language-transition",
	},
	{
		id: "reference",
		title: "Language Reference",
		path: "/learn/book/advanced",
		audience: "reference",
	},
] satisfies readonly ContentCollection[];

export const contentEntries = [
	...bookParts.map((part) => ({
		id: part.id,
		collection: collectionForPart(part.id),
		path: part.path,
		aliases: part.aliases ?? [],
		sourcePath: part.sourcePath,
		partId: part.id,
	})),
	...bookSections.map((section) => ({
		id: section.id,
		collection: collectionForPart(section.partId),
		path: section.path,
		aliases: section.aliases ?? [],
		sourcePath: sectionSourcePath(section),
		partId: section.partId,
		sectionId: section.id,
	})),
	...bookPages.map((page) => ({
		id: page.id,
		collection: collectionForPart(page.partId),
		path: page.path,
		aliases: page.aliases,
		sourcePath: page.sourcePath,
		partId: page.partId,
		sectionId: page.sectionId,
	})),
] satisfies readonly ContentEntry[];

export const languageGuideEntries = contentEntries.filter(
	(entry) => entry.collection === "guides" && entry.partId === "developers",
);
