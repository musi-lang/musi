import type { BookPartDefinition } from "./types";

export const bookParts = [
	{
		id: "start",
		path: "/docs/book/start",
		aliases: ["/docs/language/start", "/learn/language/start"],
		sourcePath: "www/src/content/book/language/start/index.md",
	},
	{
		id: "core",
		path: "/docs/book/core",
		aliases: ["/docs/language/core", "/learn/language/core"],
		sourcePath: "www/src/content/book/language/core/index.md",
	},
	{
		id: "data",
		path: "/docs/book/data",
		aliases: ["/docs/language/data", "/learn/language/data"],
		sourcePath: "www/src/content/book/language/data/index.md",
	},
	{
		id: "organization",
		path: "/docs/book/organization",
		aliases: ["/docs/language/organization", "/learn/language/organization"],
		sourcePath: "www/src/content/book/language/organization/index.md",
	},
	{
		id: "types",
		path: "/docs/book/types",
		aliases: ["/docs/language/types", "/learn/language/types"],
		sourcePath: "www/src/content/book/language/types/index.md",
	},
	{
		id: "abstractions",
		path: "/docs/book/abstractions",
		aliases: ["/docs/language/abstractions", "/learn/language/abstractions"],
		sourcePath: "www/src/content/book/language/abstractions/index.md",
	},
	{
		id: "effects-runtime",
		path: "/docs/book/effects-runtime",
		aliases: [
			"/docs/language/effects-runtime",
			"/learn/language/effects-runtime",
		],
		sourcePath: "www/src/content/book/language/effects-runtime/index.md",
	},
	{
		id: "developers",
		path: "/docs/guides",
		aliases: [
			"/docs/language/developers",
			"/learn/language/developers",
			"/learn/guides",
		],
		sourcePath: "www/src/content/book/language/developers/index.md",
	},
	{
		id: "advanced",
		path: "/docs/book/advanced",
		aliases: ["/docs/language/advanced", "/learn/language/advanced"],
		sourcePath: "www/src/content/book/language/advanced/index.md",
	},
] satisfies readonly BookPartDefinition[];
