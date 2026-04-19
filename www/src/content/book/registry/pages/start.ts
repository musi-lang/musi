import type { RawBookPageDefinition } from "../types";

export const startPages = [
	{
		id: "getting-started",
		partId: "start",
		path: "/docs/book/start/getting-started",
		aliases: [
			"/docs/language/start/getting-started",
			"/learn/language/start/getting-started",
		],
		sourcePath: "docs/what/language/start/getting-started.md",
		questions: [],
	},
	{
		id: "first-program",
		partId: "start",
		path: "/docs/book/start/first-program",
		aliases: [
			"/docs/language/start/first-program",
			"/learn/language/start/first-program",
		],
		sourcePath: "docs/what/language/start/first-program.md",
		questions: [],
	},
	{
		id: "reading-musi-code",
		partId: "start",
		path: "/docs/book/start/reading-musi-code",
		aliases: [
			"/docs/language/start/reading-musi-code",
			"/learn/language/start/reading-musi-code",
		],
		sourcePath: "docs/what/language/start/reading-musi-code.md",
		questions: [],
	},
	{
		id: "values-and-let",
		partId: "start",
		path: "/docs/book/start/values-and-let",
		aliases: [
			"/docs/language/start/values-and-let",
			"/learn/language/start/values-and-let",
		],
		sourcePath: "docs/what/language/start/values-and-let.md",
		questions: [],
	},
	{
		id: "blocks-and-expressions",
		partId: "start",
		path: "/docs/book/start/blocks-and-expressions",
		aliases: [
			"/docs/language/start/blocks-and-expressions",
			"/learn/language/start/blocks-and-expressions",
		],
		sourcePath: "docs/what/language/start/blocks-and-expressions.md",
		questions: [],
	},
	{
		id: "mutation",
		partId: "start",
		path: "/docs/book/start/mutation",
		aliases: [
			"/docs/language/start/mutation",
			"/learn/language/start/mutation",
		],
		sourcePath: "docs/what/language/start/mutation.md",
		questions: [],
	},
] satisfies readonly RawBookPageDefinition[];
