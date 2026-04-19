import type { RawBookPageDefinition } from "../types";

export const dataPages = [
	{
		id: "records",
		partId: "data",
		path: "/docs/book/data/records",
		aliases: ["/docs/language/data/records", "/learn/language/data/records"],
		sourcePath: "docs/what/language/data/records.md",
		questions: [],
	},
	{
		id: "indexing-and-fields",
		partId: "data",
		path: "/docs/book/data/indexing-and-fields",
		aliases: [
			"/docs/language/data/indexing-and-fields",
			"/learn/language/data/indexing-and-fields",
		],
		sourcePath: "docs/what/language/data/indexing-and-fields.md",
		questions: [],
	},
	{
		id: "data-definitions",
		partId: "data",
		path: "/docs/book/data/data-definitions",
		aliases: [
			"/docs/language/data/data-definitions",
			"/learn/language/data/data-definitions",
		],
		sourcePath: "docs/what/language/data/data-definitions.md",
		questions: [],
	},
	{
		id: "arrays-and-slices",
		partId: "data",
		path: "/docs/book/data/arrays-and-slices",
		aliases: [
			"/docs/language/data/arrays-and-slices",
			"/learn/language/data/arrays-and-slices",
		],
		sourcePath: "docs/what/language/data/arrays-and-slices.md",
		questions: [],
	},
	{
		id: "patterns",
		partId: "data",
		path: "/docs/book/data/patterns",
		aliases: ["/docs/language/data/patterns", "/learn/language/data/patterns"],
		sourcePath: "docs/what/language/data/patterns.md",
		questions: [],
	},
	{
		id: "option-and-result",
		partId: "data",
		path: "/docs/book/data/option-and-result",
		aliases: [
			"/docs/language/data/option-and-result",
			"/learn/language/data/option-and-result",
		],
		sourcePath: "docs/what/language/data/option-and-result.md",
		questions: [],
	},
] satisfies readonly RawBookPageDefinition[];
