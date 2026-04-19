import { rawBookPages } from "./registry/pages";
import { bookParts } from "./registry/parts";
import { bookSections } from "./registry/sections";
import type {
	BookPageDefinition,
	RawBookPageDefinition,
} from "./registry/types";

export type {
	BookPageDefinition,
	BookPageQuestion,
	BookPartDefinition,
	BookSectionDefinition,
} from "./registry/types";
export { bookParts, bookSections };

type BookSectionId = (typeof bookSections)[number]["id"];

function developerSectionIdForPage(
	page: RawBookPageDefinition,
): BookSectionId | null {
	if (page.id === "musi-for-rust-developers" || page.id.startsWith("rust-")) {
		return "developers-rust";
	}
	if (page.id.startsWith("c99-")) {
		return "developers-c99";
	}
	if (page.id.startsWith("cpp17-")) {
		return "developers-cpp17";
	}
	if (page.id.startsWith("csharp-")) {
		return "developers-csharp";
	}
	if (page.id.startsWith("go-")) {
		return "developers-go";
	}
	if (page.id.startsWith("java-")) {
		return "developers-java";
	}
	if (page.id.startsWith("js-ts-")) {
		return "developers-javascript-typescript";
	}
	if (page.id.startsWith("lua-")) {
		return "developers-lua";
	}
	if (page.id.startsWith("python-")) {
		return "developers-python";
	}
	return "developers-guides";
}

function sectionIdForPage(page: RawBookPageDefinition): BookSectionId | null {
	switch (page.partId) {
		case "start":
			return null;
		case "core":
			return null;
		case "data":
			return null;
		case "organization":
			return null;
		case "types":
			return null;
		case "abstractions":
			return null;
		case "effects-runtime":
			return null;
		case "developers":
			return developerSectionIdForPage(page);
		case "advanced":
			return null;
		default:
			throw new Error(`unknown part ${page.partId} for page ${page.id}`);
	}
}

export const bookPages = rawBookPages.map((page) => ({
	...page,
	sectionId: sectionIdForPage(page),
})) satisfies readonly BookPageDefinition[];
