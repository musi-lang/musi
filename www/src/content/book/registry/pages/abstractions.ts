import type { RawBookPageDefinition } from "../types";

export const abstractionsPages = [
	{
		id: "classes",
		partId: "abstractions",
		path: "/docs/book/abstractions/classes",
		aliases: [
			"/docs/language/abstractions/classes",
			"/learn/language/abstractions/classes",
		],
		sourcePath: "docs/what/language/abstractions/classes.md",
		questions: [],
	},
	{
		id: "instances",
		partId: "abstractions",
		path: "/docs/book/abstractions/instances",
		aliases: [
			"/docs/language/abstractions/instances",
			"/learn/language/abstractions/instances",
		],
		sourcePath: "docs/what/language/abstractions/instances.md",
		questions: [],
	},
	{
		id: "laws",
		partId: "abstractions",
		path: "/docs/book/abstractions/laws",
		aliases: [
			"/docs/language/abstractions/laws",
			"/learn/language/abstractions/laws",
		],
		sourcePath: "docs/what/language/abstractions/laws.md",
		questions: [],
	},
] satisfies readonly RawBookPageDefinition[];
