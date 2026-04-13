import type { Locale } from "../../lib/site-copy";

export interface BookPartDefinition {
	id: string;
	path: string;
	sourcePath: string;
}

export interface BookPageQuestion {
	labels: Record<Locale, string>;
}

export interface BookPageDefinition {
	id: string;
	partId: string;
	path: string;
	aliases: string[];
	sourcePath: string;
	questions: BookPageQuestion[];
}

export const bookParts = [
	{
		id: "start",
		path: "/docs/start",
		sourcePath: "src/content/book/start/index.md",
	},
	{
		id: "core-language",
		path: "/docs/core-language",
		sourcePath: "src/content/book/core-language/index.md",
	},
	{
		id: "types-and-abstractions",
		path: "/docs/types-and-abstractions",
		sourcePath: "src/content/book/types-and-abstractions/index.md",
	},
	{
		id: "tooling",
		path: "/docs/tooling",
		sourcePath: "src/content/book/tooling/index.md",
	},
	{
		id: "questions",
		path: "/docs/questions",
		sourcePath: "src/content/book/questions/index.md",
	},
] satisfies readonly BookPartDefinition[];

export const bookPages = [
	{
		id: "getting-started",
		partId: "start",
		path: "/docs/start/getting-started",
		aliases: ["/docs/getting-started"],
		sourcePath: "src/content/docs/getting-started.md",
		questions: [
			{
				labels: {
					en: "How do I install Musi and learn the <code>music</code> / <code>musi</code> split?",
					ja: "Musi を導入して <code>music</code> と <code>musi</code> の違いを知るには?",
				},
			},
		],
	},
	{
		id: "first-program",
		partId: "start",
		path: "/docs/start/first-program",
		aliases: ["/docs/first-program"],
		sourcePath: "src/content/docs/first-program.md",
		questions: [
			{
				labels: {
					en: "How do I write and run the smallest Musi file?",
					ja: "最小の Musi file を書いて実行するには?",
				},
			},
		],
	},
	{
		id: "files-packages-and-entry",
		partId: "start",
		path: "/docs/start/files-packages-and-entry",
		aliases: ["/docs/files-packages-and-entry"],
		sourcePath: "src/content/docs/files-packages-and-entry.md",
		questions: [
			{
				labels: {
					en: "How do I switch from one file to a package?",
					ja: "1 file から package へ切り替えるには?",
				},
			},
		],
	},
	{
		id: "imports-and-packages",
		partId: "start",
		path: "/docs/start/imports-and-packages",
		aliases: ["/docs/imports-and-packages"],
		sourcePath: "src/content/docs/imports-and-packages.md",
		questions: [
			{
				labels: {
					en: "How do I import <code>@std</code> modules and when do I use <code>musi:*</code>?",
					ja: "<code>@std</code> module を import し、<code>musi:*</code> を使う場面を知るには?",
				},
			},
		],
	},
	{
		id: "expressions-and-bindings",
		partId: "core-language",
		path: "/docs/core-language/expressions-and-bindings",
		aliases: ["/docs/expressions-and-bindings"],
		sourcePath: "src/content/docs/expressions-and-bindings.md",
		questions: [
			{
				labels: {
					en: "How do I read a Musi file top to bottom?",
					ja: "Musi file を上から下へ読むには?",
				},
			},
		],
	},
	{
		id: "operators-and-literals",
		partId: "core-language",
		path: "/docs/core-language/operators-and-literals",
		aliases: ["/docs/operators-and-literals"],
		sourcePath: "src/content/docs/operators-and-literals.md",
		questions: [
			{
				labels: {
					en: "How do operators, records, and arrays fit in ordinary expressions?",
					ja: "operator、record、array は通常の式の中でどう書く?",
				},
			},
		],
	},
	{
		id: "functions-and-calls",
		partId: "core-language",
		path: "/docs/core-language/functions-and-calls",
		aliases: ["/docs/functions-and-calls"],
		sourcePath: "src/content/docs/functions-and-calls.md",
		questions: [
			{
				labels: {
					en: "How do I define functions and recursion with <code>let</code>?",
					ja: "<code>let</code> で function と recursion を定義するには?",
				},
			},
		],
	},
	{
		id: "data-and-pattern-matching",
		partId: "core-language",
		path: "/docs/core-language/data-and-pattern-matching",
		aliases: ["/docs/data-and-pattern-matching"],
		sourcePath: "src/content/docs/data-and-pattern-matching.md",
		questions: [
			{
				labels: {
					en: "How do I model variants and branch with <code>case</code>?",
					ja: "<code>case</code> で variant を表し分岐するには?",
				},
			},
		],
	},
	{
		id: "records-arrays-and-mutation",
		partId: "core-language",
		path: "/docs/core-language/records-arrays-and-mutation",
		aliases: ["/docs/records-arrays-and-mutation"],
		sourcePath: "src/content/docs/records-arrays-and-mutation.md",
		questions: [
			{
				labels: {
					en: "How do I update records and arrays without guessing syntax?",
					ja: "record と array を構文で迷わず更新するには?",
				},
			},
		],
	},
	{
		id: "types",
		partId: "types-and-abstractions",
		path: "/docs/types-and-abstractions/types",
		aliases: ["/docs/types"],
		sourcePath: "src/content/docs/types.md",
		questions: [
			{
				labels: {
					en: "How do I add type annotations and generics?",
					ja: "型注釈と generic を追加するには?",
				},
			},
		],
	},
	{
		id: "classes-instances-and-laws",
		partId: "types-and-abstractions",
		path: "/docs/types-and-abstractions/classes-instances-and-laws",
		aliases: ["/docs/classes-instances-and-laws"],
		sourcePath: "src/content/docs/classes-instances-and-laws.md",
		questions: [
			{
				labels: {
					en: "How do I define a class and add an instance?",
					ja: "class を定義して instance を追加するには?",
				},
			},
		],
	},
	{
		id: "effects-and-handlers",
		partId: "types-and-abstractions",
		path: "/docs/types-and-abstractions/effects-and-handlers",
		aliases: ["/docs/effects-and-handlers"],
		sourcePath: "src/content/docs/effects-and-handlers.md",
		questions: [
			{
				labels: {
					en: "How do I define effects, handle them, and use <code>resume</code>?",
					ja: "effect を定義し、handle し、<code>resume</code> を使うには?",
				},
			},
		],
	},
	{
		id: "quote-and-syntax",
		partId: "types-and-abstractions",
		path: "/docs/types-and-abstractions/quote-and-syntax",
		aliases: ["/docs/quote-and-syntax"],
		sourcePath: "src/content/docs/quote-and-syntax.md",
		questions: [
			{
				labels: {
					en: "How do <code>quote</code> and splice work in real Musi code?",
					ja: "実際の Musi code で <code>quote</code> と splice はどう使う?",
				},
			},
		],
	},
	{
		id: "attributes-and-foreign",
		partId: "types-and-abstractions",
		path: "/docs/types-and-abstractions/attributes-and-foreign",
		aliases: ["/docs/attributes-and-foreign"],
		sourcePath: "src/content/docs/attributes-and-foreign.md",
		questions: [
			{
				labels: {
					en: "How do I declare foreign functions and attributes?",
					ja: "foreign function と attribute を宣言するには?",
				},
			},
		],
	},
	{
		id: "foundation-and-standard-library",
		partId: "tooling",
		path: "/docs/tooling/foundation-and-standard-library",
		aliases: ["/docs/foundation-and-standard-library"],
		sourcePath: "src/content/docs/foundation-and-standard-library.md",
		questions: [
			{
				labels: {
					en: "How do <code>@std</code> and <code>musi:*</code> differ?",
					ja: "<code>@std</code> と <code>musi:*</code> はどう違う?",
				},
			},
		],
	},
	{
		id: "testing-and-running",
		partId: "tooling",
		path: "/docs/tooling/testing-and-running",
		aliases: ["/docs/testing-and-running"],
		sourcePath: "src/content/docs/testing-and-running.md",
		questions: [
			{
				labels: {
					en: "How do I run tests, packages, and direct files?",
					ja: "test、package、direct file を実行するには?",
				},
			},
		],
	},
	{
		id: "common-questions",
		partId: "questions",
		path: "/docs/questions/common-questions",
		aliases: ["/docs/common-questions"],
		sourcePath: "src/content/book/questions/common-questions.md",
		questions: [],
	},
] satisfies readonly BookPageDefinition[];
