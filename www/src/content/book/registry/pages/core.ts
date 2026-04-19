import type { RawBookPageDefinition } from "../types";

export const corePages = [
	{
		id: "literals",
		partId: "core",
		path: "/docs/book/core/literals",
		aliases: ["/docs/language/core/literals", "/learn/language/core/literals"],
		sourcePath: "docs/what/language/core/literals.md",
		questions: [],
	},
	{
		id: "tuples-and-unit",
		partId: "core",
		path: "/docs/book/core/tuples-and-unit",
		aliases: [
			"/docs/language/core/tuples-and-unit",
			"/learn/language/core/tuples-and-unit",
		],
		sourcePath: "docs/what/language/core/tuples-and-unit.md",
		questions: [],
	},
	{
		id: "operators",
		partId: "core",
		path: "/docs/book/core/operators",
		aliases: [
			"/docs/language/core/operators",
			"/learn/language/core/operators",
		],
		sourcePath: "docs/what/language/core/operators.md",
		questions: [],
	},
	{
		id: "ranges",
		partId: "core",
		path: "/docs/book/core/ranges",
		aliases: ["/docs/language/core/ranges", "/learn/language/core/ranges"],
		sourcePath: "docs/what/language/core/ranges.md",
		questions: [],
	},
	{
		id: "functions",
		partId: "core",
		path: "/docs/book/core/functions",
		aliases: [
			"/docs/language/core/functions",
			"/learn/language/core/functions",
		],
		sourcePath: "docs/what/language/core/functions.md",
		questions: [],
	},
	{
		id: "lambdas",
		partId: "core",
		path: "/docs/book/core/lambdas",
		aliases: ["/docs/language/core/lambdas", "/learn/language/core/lambdas"],
		sourcePath: "docs/what/language/core/lambdas.md",
		questions: [],
	},
	{
		id: "calls",
		partId: "core",
		path: "/docs/book/core/calls",
		aliases: ["/docs/language/core/calls", "/learn/language/core/calls"],
		sourcePath: "docs/what/language/core/calls.md",
		questions: [],
	},
	{
		id: "dot-calls",
		partId: "core",
		path: "/docs/book/core/dot-calls",
		aliases: [
			"/docs/book/core/methods",
			"/docs/language/core/methods",
			"/learn/language/core/methods",
		],
		sourcePath: "docs/what/language/core/methods.md",
		questions: [],
	},
] satisfies readonly RawBookPageDefinition[];
