export interface BookPartDefinition {
	id: string;
	path: string;
	sourcePath: string;
	aliases?: string[];
}

export interface BookSectionDefinition {
	id: string;
	partId: string;
	parentId?: string;
	path: string;
	aliases?: string[];
	title: string;
	description: string;
	group: string;
	section: string;
	order: number;
	slug: string;
	summary: string;
	sourcePath?: string;
}

export interface BookPageQuestion {
	labels: Record<"en", string>;
}

export interface BookPageDefinition {
	id: string;
	partId: string;
	sectionId: string;
	path: string;
	aliases: string[];
	sourcePath: string;
	questions: BookPageQuestion[];
}

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
		path: "/docs/book/developers",
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

export const bookSections: readonly BookSectionDefinition[] = [
	{
		id: "start-foundations",
		partId: "start",
		path: "/docs/book/start/foundations",
		aliases: [
			"/docs/language/start/foundations",
			"/learn/language/start/foundations",
		],
		title: "Foundations",
		description:
			"Install Musi, read one file, and learn expression-first flow.",
		group: "Start",
		section: "Foundations",
		order: 1,
		slug: "foundations",
		summary:
			"First setup and first-file habits: values, blocks, and explicit mutation.",
	},
	{
		id: "core-expressions",
		partId: "core",
		path: "/docs/book/core/expressions",
		aliases: [
			"/docs/language/core/expressions",
			"/learn/language/core/expressions",
		],
		title: "Expressions",
		description:
			"Learn literals, tuples, operators, and ranges for everyday calculations.",
		group: "Core Syntax",
		section: "Expressions",
		order: 1,
		slug: "expressions",
		summary: "Core value and operator forms used across normal Musi code.",
	},
	{
		id: "core-functions",
		partId: "core",
		path: "/docs/book/core/functions-and-calls",
		aliases: [
			"/docs/language/core/functions-and-calls",
			"/learn/language/core/functions-and-calls",
		],
		title: "Functions and Calls",
		description:
			"Define reusable functions, write lambdas, and choose call and method forms.",
		group: "Core Syntax",
		section: "Functions and Calls",
		order: 2,
		slug: "functions-and-calls",
		summary: "Function declarations, lambdas, and call-site reading patterns.",
	},
	{
		id: "data-modeling",
		partId: "data",
		path: "/docs/book/data/modeling",
		aliases: ["/docs/language/data/modeling", "/learn/language/data/modeling"],
		title: "Data Modeling",
		description: "Model records, variants, collections, and matching patterns.",
		group: "Data and Patterns",
		section: "Data Modeling",
		order: 1,
		slug: "modeling",
		summary:
			"Build domain data with records and variants, then read and transform with patterns.",
	},
	{
		id: "organization-modules",
		partId: "organization",
		path: "/docs/book/organization/modules",
		aliases: [
			"/docs/language/organization/modules",
			"/learn/language/organization/modules",
		],
		title: "Files and Modules",
		description:
			"Structure files, packages, and imports so projects stay navigable.",
		group: "Organization and Modules",
		section: "Files and Modules",
		order: 1,
		slug: "modules",
		summary:
			"Project layout, package boundaries, and import/export discipline.",
	},
	{
		id: "types-foundations",
		partId: "types",
		path: "/docs/book/types/foundations",
		aliases: [
			"/docs/language/types/foundations",
			"/learn/language/types/foundations",
		],
		title: "Type Foundations",
		description:
			"Type annotations, inference, generics, callable types, and checks.",
		group: "Types and Generic Design",
		section: "Type Foundations",
		order: 1,
		slug: "foundations",
		summary:
			"Type reading and design patterns from everyday annotations to advanced forms.",
	},
	{
		id: "abstractions-laws",
		partId: "abstractions",
		path: "/docs/book/abstractions/classes-instances-laws",
		aliases: [
			"/docs/language/abstractions/classes-instances-laws",
			"/learn/language/abstractions/classes-instances-laws",
		],
		title: "Classes, Instances, and Laws",
		description:
			"Define reusable interfaces, implementations, and behavior constraints.",
		group: "Abstractions",
		section: "Classes and Laws",
		order: 1,
		slug: "classes-instances-laws",
		summary:
			"Shared behavior contracts and law-oriented implementation discipline.",
	},
	{
		id: "effects-handling",
		partId: "effects-runtime",
		path: "/docs/book/effects-runtime/handling",
		aliases: [
			"/docs/language/effects-runtime/handling",
			"/learn/language/effects-runtime/handling",
		],
		title: "Effect Handling",
		description:
			"Request operations and handle them with explicit handler scopes.",
		group: "Effects and Runtime",
		section: "Effect Handling",
		order: 1,
		slug: "handling",
		summary: "Effect requests, using scopes, and handler composition.",
	},
	{
		id: "effects-runtime-model",
		partId: "effects-runtime",
		path: "/docs/book/effects-runtime/runtime-model",
		aliases: [
			"/docs/language/effects-runtime/runtime-model",
			"/learn/language/effects-runtime/runtime-model",
		],
		title: "Runtime Model",
		description: "Understand runtime boundaries and stdlib effect patterns.",
		group: "Effects and Runtime",
		section: "Runtime Model",
		order: 2,
		slug: "runtime-model",
		summary:
			"Runtime behavior, operational model, and standard library integration.",
	},
	{
		id: "developers-guides",
		partId: "developers",
		path: "/docs/book/developers/guides",
		aliases: [
			"/docs/language/developers/guides",
			"/learn/language/developers/guides",
		],
		title: "Language Guides",
		description:
			"Map existing language habits to Musi with practical side-by-side guidance.",
		group: "Musi for Developers",
		section: "Language Guides",
		order: 1,
		slug: "guides",
		summary:
			"Comparative guides for teams moving from other language ecosystems.",
	},
	{
		id: "developers-rust",
		partId: "developers",
		parentId: "developers-guides",
		path: "/docs/book/developers/guides/rust",
		aliases: [
			"/docs/book/developers/rust",
			"/docs/language/developers/rust-guide",
			"/learn/language/developers/rust-guide",
		],
		title: "Musi for Rust Developers",
		description:
			"Translate Rust habits into Musi's expression, data, abstraction, and mutation model.",
		group: "Musi for Developers",
		section: "Rust Developers",
		order: 1,
		slug: "rust",
		summary:
			"Rust-specific contrasts for mutation, data, traits, effects, and native boundaries.",
	},
	{
		id: "developers-javascript-typescript",
		partId: "developers",
		parentId: "developers-guides",
		path: "/docs/book/developers/guides/javascript-typescript",
		aliases: [
			"/docs/book/developers/guides/javascript",
			"/docs/book/developers/guides/typescript",
			"/docs/book/developers/javascript",
			"/docs/book/developers/typescript",
			"/docs/language/developers/javascript",
			"/docs/language/developers/typescript",
			"/learn/language/developers/javascript",
			"/learn/language/developers/typescript",
		],
		title: "Musi for JavaScript and TypeScript Developers",
		description:
			"Translate JavaScript runtime habits and TypeScript 6.0.2 type habits into Musi.",
		group: "Musi for Developers",
		section: "JavaScript and TypeScript Developers",
		order: 1,
		slug: "javascript-typescript",
		summary:
			"Side-by-side contrasts for objects, functions, unions, generics, promises, modules, and tests.",
	},
	{
		id: "advanced-interop",
		partId: "advanced",
		path: "/docs/book/advanced/interop",
		aliases: [
			"/docs/language/advanced/interop",
			"/learn/language/advanced/interop",
		],
		title: "Interop and Safety",
		description:
			"Use attributes, foreign declarations, and unsafe FFI boundaries.",
		group: "Advanced and Tooling",
		section: "Interop and Safety",
		order: 1,
		slug: "interop",
		summary: "Native interop boundaries and explicit unsafe operations.",
	},
	{
		id: "advanced-meta-tooling",
		partId: "advanced",
		path: "/docs/book/advanced/meta-tooling",
		aliases: [
			"/docs/language/advanced/meta-tooling",
			"/learn/language/advanced/meta-tooling",
		],
		title: "Meta and Tooling",
		description:
			"Operator declarations, syntax tools, templates, testing, and run flow.",
		group: "Advanced and Tooling",
		section: "Meta and Tooling",
		order: 2,
		slug: "meta-tooling",
		summary:
			"Syntax-oriented authoring tools and day-to-day development workflow.",
	},
];

const rawBookPages = [
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
		id: "methods",
		partId: "core",
		path: "/docs/book/core/methods",
		aliases: ["/docs/language/core/methods", "/learn/language/core/methods"],
		sourcePath: "docs/what/language/core/methods.md",
		questions: [],
	},
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
		id: "files",
		partId: "organization",
		path: "/docs/book/organization/files",
		aliases: [
			"/docs/language/organization/files",
			"/learn/language/organization/files",
		],
		sourcePath: "docs/what/language/organization/files.md",
		questions: [],
	},
	{
		id: "packages",
		partId: "organization",
		path: "/docs/book/organization/packages",
		aliases: [
			"/docs/language/organization/packages",
			"/learn/language/organization/packages",
		],
		sourcePath: "docs/what/language/organization/packages.md",
		questions: [],
	},
	{
		id: "imports-and-exports",
		partId: "organization",
		path: "/docs/book/organization/imports-and-exports",
		aliases: [
			"/docs/language/organization/imports-and-exports",
			"/learn/language/organization/imports-and-exports",
		],
		sourcePath: "docs/what/language/organization/imports-and-exports.md",
		questions: [],
	},
	{
		id: "type-annotations",
		partId: "types",
		path: "/docs/book/types/type-annotations",
		aliases: [
			"/docs/language/types/type-annotations",
			"/learn/language/types/type-annotations",
		],
		sourcePath: "docs/what/language/types/type-annotations.md",
		questions: [],
	},
	{
		id: "callable-types",
		partId: "types",
		path: "/docs/book/types/callable-types",
		aliases: [
			"/docs/language/types/callable-types",
			"/learn/language/types/callable-types",
		],
		sourcePath: "docs/what/language/types/callable-types.md",
		questions: [],
	},
	{
		id: "type-inference",
		partId: "types",
		path: "/docs/book/types/type-inference",
		aliases: [
			"/docs/language/types/type-inference",
			"/learn/language/types/type-inference",
		],
		sourcePath: "docs/what/language/types/type-inference.md",
		questions: [],
	},
	{
		id: "generics",
		partId: "types",
		path: "/docs/book/types/generics",
		aliases: [
			"/docs/language/types/generics",
			"/learn/language/types/generics",
		],
		sourcePath: "docs/what/language/types/generics.md",
		questions: [],
	},
	{
		id: "type-tests-and-casts",
		partId: "types",
		path: "/docs/book/types/type-tests-and-casts",
		aliases: [
			"/docs/language/types/type-tests-and-casts",
			"/learn/language/types/type-tests-and-casts",
		],
		sourcePath: "docs/what/language/types/type-tests-and-casts.md",
		questions: [],
	},
	{
		id: "forall-types",
		partId: "types",
		path: "/docs/book/types/forall-types",
		aliases: [
			"/docs/language/types/forall-types",
			"/learn/language/types/forall-types",
		],
		sourcePath: "docs/what/language/types/forall-types.md",
		questions: [],
	},
	{
		id: "dependent-types",
		partId: "types",
		path: "/docs/book/types/dependent-types",
		aliases: [
			"/docs/language/types/dependent-types",
			"/learn/language/types/dependent-types",
		],
		sourcePath: "docs/what/language/types/dependent-types.md",
		questions: [],
	},
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
	{
		id: "effects",
		partId: "effects-runtime",
		path: "/docs/book/effects-runtime/effects",
		aliases: [
			"/docs/language/effects-runtime/effects",
			"/learn/language/effects-runtime/effects",
		],
		sourcePath: "docs/what/language/effects-runtime/effects.md",
		questions: [],
	},
	{
		id: "using",
		partId: "effects-runtime",
		path: "/docs/book/effects-runtime/using",
		aliases: [
			"/docs/language/effects-runtime/using",
			"/learn/language/effects-runtime/using",
		],
		sourcePath: "docs/what/language/effects-runtime/using.md",
		questions: [],
	},
	{
		id: "handlers",
		partId: "effects-runtime",
		path: "/docs/book/effects-runtime/handlers",
		aliases: [
			"/docs/language/effects-runtime/handlers",
			"/learn/language/effects-runtime/handlers",
		],
		sourcePath: "docs/what/language/effects-runtime/handlers.md",
		questions: [],
	},
	{
		id: "foundation",
		partId: "effects-runtime",
		path: "/docs/book/effects-runtime/foundation",
		aliases: [
			"/docs/language/effects-runtime/foundation",
			"/learn/language/effects-runtime/foundation",
		],
		sourcePath: "docs/what/language/effects-runtime/foundation.md",
		questions: [],
	},
	{
		id: "runtime",
		partId: "effects-runtime",
		path: "/docs/book/effects-runtime/runtime",
		aliases: [
			"/docs/language/effects-runtime/runtime",
			"/learn/language/effects-runtime/runtime",
		],
		sourcePath: "docs/what/language/effects-runtime/runtime.md",
		questions: [],
	},
	{
		id: "stdlib",
		partId: "effects-runtime",
		path: "/docs/book/effects-runtime/stdlib",
		aliases: [
			"/docs/language/effects-runtime/stdlib",
			"/learn/language/effects-runtime/stdlib",
		],
		sourcePath: "docs/what/language/effects-runtime/stdlib.md",
		questions: [],
	},
	{
		id: "attributes",
		partId: "advanced",
		path: "/docs/book/advanced/attributes",
		aliases: [
			"/docs/language/advanced/attributes",
			"/learn/language/advanced/attributes",
		],
		sourcePath: "docs/what/language/advanced/attributes.md",
		questions: [],
	},
	{
		id: "foreign",
		partId: "advanced",
		path: "/docs/book/advanced/foreign",
		aliases: [
			"/docs/language/advanced/foreign",
			"/learn/language/advanced/foreign",
		],
		sourcePath: "docs/what/language/advanced/foreign.md",
		questions: [],
	},
	{
		id: "unsafe-and-ffi",
		partId: "advanced",
		path: "/docs/book/advanced/unsafe-and-ffi",
		aliases: [
			"/docs/language/advanced/unsafe-and-ffi",
			"/learn/language/advanced/unsafe-and-ffi",
		],
		sourcePath: "docs/what/language/advanced/unsafe-and-ffi.md",
		questions: [],
	},
	{
		id: "operator-forms",
		partId: "advanced",
		path: "/docs/book/advanced/operator-forms",
		aliases: [
			"/docs/language/advanced/operator-forms",
			"/learn/language/advanced/operator-forms",
		],
		sourcePath: "docs/what/language/advanced/operator-forms.md",
		questions: [],
	},
	{
		id: "quote-and-syntax",
		partId: "advanced",
		path: "/docs/book/advanced/quote-and-syntax",
		aliases: [
			"/docs/language/advanced/quote-and-syntax",
			"/learn/language/advanced/quote-and-syntax",
		],
		sourcePath: "docs/what/language/advanced/quote-and-syntax.md",
		questions: [],
	},
	{
		id: "templates-and-splices",
		partId: "advanced",
		path: "/docs/book/advanced/templates-and-splices",
		aliases: [
			"/docs/language/advanced/templates-and-splices",
			"/learn/language/advanced/templates-and-splices",
		],
		sourcePath: "docs/what/language/advanced/templates-and-splices.md",
		questions: [],
	},
	{
		id: "testing",
		partId: "advanced",
		path: "/docs/book/advanced/testing",
		aliases: [
			"/docs/language/advanced/testing",
			"/learn/language/advanced/testing",
		],
		sourcePath: "docs/what/language/advanced/testing.md",
		questions: [],
	},
	{
		id: "running-and-tooling",
		partId: "advanced",
		path: "/docs/book/advanced/running-and-tooling",
		aliases: [
			"/docs/language/advanced/running-and-tooling",
			"/learn/language/advanced/running-and-tooling",
		],
		sourcePath: "docs/what/language/advanced/running-and-tooling.md",
		questions: [],
	},
	{
		id: "js-ts-overview",
		partId: "developers",
		path: "/docs/book/developers/javascript-typescript/overview",
		aliases: [
			"/docs/book/developers/javascript/overview",
			"/docs/book/developers/typescript/overview",
			"/docs/language/developers/javascript/overview",
			"/docs/language/developers/typescript/overview",
			"/learn/language/developers/javascript/overview",
			"/learn/language/developers/typescript/overview",
		],
		sourcePath:
			"docs/what/language/developers/javascript-typescript/overview.md",
		questions: [],
	},
	{
		id: "js-ts-values-functions",
		partId: "developers",
		path: "/docs/book/developers/javascript-typescript/values-functions",
		aliases: [
			"/docs/book/developers/javascript/values-functions",
			"/docs/book/developers/typescript/values-functions",
			"/docs/language/developers/javascript/values-functions",
			"/docs/language/developers/typescript/values-functions",
			"/learn/language/developers/javascript/values-functions",
			"/learn/language/developers/typescript/values-functions",
		],
		sourcePath:
			"docs/what/language/developers/javascript-typescript/values-functions.md",
		questions: [],
	},
	{
		id: "js-ts-state",
		partId: "developers",
		path: "/docs/book/developers/javascript-typescript/state",
		aliases: [
			"/docs/book/developers/javascript/state",
			"/docs/book/developers/typescript/state",
			"/docs/language/developers/javascript/state",
			"/docs/language/developers/typescript/state",
			"/learn/language/developers/javascript/state",
			"/learn/language/developers/typescript/state",
		],
		sourcePath: "docs/what/language/developers/javascript-typescript/state.md",
		questions: [],
	},
	{
		id: "js-ts-objects-records",
		partId: "developers",
		path: "/docs/book/developers/javascript-typescript/objects-records",
		aliases: [
			"/docs/book/developers/javascript/objects-records",
			"/docs/book/developers/typescript/objects-records",
			"/docs/language/developers/javascript/objects-records",
			"/docs/language/developers/typescript/objects-records",
			"/learn/language/developers/javascript/objects-records",
			"/learn/language/developers/typescript/objects-records",
		],
		sourcePath:
			"docs/what/language/developers/javascript-typescript/objects-records.md",
		questions: [],
	},
	{
		id: "js-ts-arrays-pipelines",
		partId: "developers",
		path: "/docs/book/developers/javascript-typescript/arrays-pipelines",
		aliases: [
			"/docs/book/developers/javascript/arrays-pipelines",
			"/docs/book/developers/typescript/arrays-pipelines",
			"/docs/language/developers/javascript/arrays-pipelines",
			"/docs/language/developers/typescript/arrays-pipelines",
			"/learn/language/developers/javascript/arrays-pipelines",
			"/learn/language/developers/typescript/arrays-pipelines",
		],
		sourcePath:
			"docs/what/language/developers/javascript-typescript/arrays-pipelines.md",
		questions: [],
	},
	{
		id: "js-ts-null-result",
		partId: "developers",
		path: "/docs/book/developers/javascript-typescript/null-result",
		aliases: [
			"/docs/book/developers/javascript/null-result",
			"/docs/book/developers/typescript/null-result",
			"/docs/language/developers/javascript/null-result",
			"/docs/language/developers/typescript/null-result",
			"/learn/language/developers/javascript/null-result",
			"/learn/language/developers/typescript/null-result",
		],
		sourcePath:
			"docs/what/language/developers/javascript-typescript/null-result.md",
		questions: [],
	},
	{
		id: "js-ts-unions-variants",
		partId: "developers",
		path: "/docs/book/developers/javascript-typescript/unions-variants",
		aliases: [
			"/docs/book/developers/javascript/unions-variants",
			"/docs/book/developers/typescript/unions-variants",
			"/docs/language/developers/javascript/unions-variants",
			"/docs/language/developers/typescript/unions-variants",
			"/learn/language/developers/javascript/unions-variants",
			"/learn/language/developers/typescript/unions-variants",
		],
		sourcePath:
			"docs/what/language/developers/javascript-typescript/unions-variants.md",
		questions: [],
	},
	{
		id: "js-ts-generics",
		partId: "developers",
		path: "/docs/book/developers/javascript-typescript/generics",
		aliases: [
			"/docs/book/developers/javascript/generics",
			"/docs/book/developers/typescript/generics",
			"/docs/language/developers/javascript/generics",
			"/docs/language/developers/typescript/generics",
			"/learn/language/developers/javascript/generics",
			"/learn/language/developers/typescript/generics",
		],
		sourcePath:
			"docs/what/language/developers/javascript-typescript/generics.md",
		questions: [],
	},
	{
		id: "js-ts-promises-effects",
		partId: "developers",
		path: "/docs/book/developers/javascript-typescript/promises-effects",
		aliases: [
			"/docs/book/developers/javascript/promises-effects",
			"/docs/book/developers/typescript/promises-effects",
			"/docs/language/developers/javascript/promises-effects",
			"/docs/language/developers/typescript/promises-effects",
			"/learn/language/developers/javascript/promises-effects",
			"/learn/language/developers/typescript/promises-effects",
		],
		sourcePath:
			"docs/what/language/developers/javascript-typescript/promises-effects.md",
		questions: [],
	},
	{
		id: "js-ts-modules-packages",
		partId: "developers",
		path: "/docs/book/developers/javascript-typescript/modules-packages",
		aliases: [
			"/docs/book/developers/javascript/modules-packages",
			"/docs/book/developers/typescript/modules-packages",
			"/docs/language/developers/javascript/modules-packages",
			"/docs/language/developers/typescript/modules-packages",
			"/learn/language/developers/javascript/modules-packages",
			"/learn/language/developers/typescript/modules-packages",
		],
		sourcePath:
			"docs/what/language/developers/javascript-typescript/modules-packages.md",
		questions: [],
	},
	{
		id: "js-ts-classes-behavior",
		partId: "developers",
		path: "/docs/book/developers/javascript-typescript/classes-behavior",
		aliases: [
			"/docs/book/developers/javascript/classes-behavior",
			"/docs/book/developers/typescript/classes-behavior",
			"/docs/language/developers/javascript/classes-behavior",
			"/docs/language/developers/typescript/classes-behavior",
			"/learn/language/developers/javascript/classes-behavior",
			"/learn/language/developers/typescript/classes-behavior",
		],
		sourcePath:
			"docs/what/language/developers/javascript-typescript/classes-behavior.md",
		questions: [],
	},
	{
		id: "js-ts-testing-tooling",
		partId: "developers",
		path: "/docs/book/developers/javascript-typescript/testing-tooling",
		aliases: [
			"/docs/book/developers/javascript/testing-tooling",
			"/docs/book/developers/typescript/testing-tooling",
			"/docs/language/developers/javascript/testing-tooling",
			"/docs/language/developers/typescript/testing-tooling",
			"/learn/language/developers/javascript/testing-tooling",
			"/learn/language/developers/typescript/testing-tooling",
		],
		sourcePath:
			"docs/what/language/developers/javascript-typescript/testing-tooling.md",
		questions: [],
	},
	{
		id: "musi-for-rust-developers",
		partId: "developers",
		path: "/docs/book/developers/overview",
		aliases: [
			"/docs/book/developers/rust/overview",
			"/docs/language/developers/rust",
			"/learn/language/developers/rust",
		],
		sourcePath: "docs/what/language/developers/rust.md",
		questions: [],
	},
	{
		id: "rust-values-functions",
		partId: "developers",
		path: "/docs/book/developers/values-functions",
		aliases: [
			"/docs/book/developers/rust/values-functions",
			"/docs/language/developers/rust/values-functions",
			"/learn/language/developers/rust/values-functions",
		],
		sourcePath: "docs/what/language/developers/rust/values-functions.md",
		questions: [],
	},
	{
		id: "rust-mutation",
		partId: "developers",
		path: "/docs/book/developers/mutation",
		aliases: [
			"/docs/book/developers/rust/mutation",
			"/docs/language/developers/rust/mutation",
			"/learn/language/developers/rust/mutation",
		],
		sourcePath: "docs/what/language/developers/rust/mutation.md",
		questions: [],
	},
	{
		id: "rust-records-structs",
		partId: "developers",
		path: "/docs/book/developers/records-structs",
		aliases: [
			"/docs/book/developers/rust/records-structs",
			"/docs/language/developers/rust/records-structs",
			"/learn/language/developers/rust/records-structs",
		],
		sourcePath: "docs/what/language/developers/rust/records-structs.md",
		questions: [],
	},
	{
		id: "rust-enums-data",
		partId: "developers",
		path: "/docs/book/developers/enums-data",
		aliases: [
			"/docs/book/developers/rust/enums-data",
			"/docs/language/developers/rust/enums-data",
			"/learn/language/developers/rust/enums-data",
		],
		sourcePath: "docs/what/language/developers/rust/enums-data.md",
		questions: [],
	},
	{
		id: "rust-traits-classes-laws",
		partId: "developers",
		path: "/docs/book/developers/traits-classes-laws",
		aliases: [
			"/docs/book/developers/rust/traits-classes-laws",
			"/docs/language/developers/rust/traits-classes-laws",
			"/learn/language/developers/rust/traits-classes-laws",
		],
		sourcePath: "docs/what/language/developers/rust/traits-classes-laws.md",
		questions: [],
	},
	{
		id: "rust-generics",
		partId: "developers",
		path: "/docs/book/developers/generics",
		aliases: [
			"/docs/book/developers/rust/generics",
			"/docs/language/developers/rust/generics",
			"/learn/language/developers/rust/generics",
		],
		sourcePath: "docs/what/language/developers/rust/generics.md",
		questions: [],
	},
	{
		id: "rust-results-effects",
		partId: "developers",
		path: "/docs/book/developers/results-effects",
		aliases: [
			"/docs/book/developers/rust/results-effects",
			"/docs/language/developers/rust/results-effects",
			"/learn/language/developers/rust/results-effects",
		],
		sourcePath: "docs/what/language/developers/rust/results-effects.md",
		questions: [],
	},
	{
		id: "rust-modules-packages",
		partId: "developers",
		path: "/docs/book/developers/modules-packages",
		aliases: [
			"/docs/book/developers/rust/modules-packages",
			"/docs/language/developers/rust/modules-packages",
			"/learn/language/developers/rust/modules-packages",
		],
		sourcePath: "docs/what/language/developers/rust/modules-packages.md",
		questions: [],
	},
	{
		id: "rust-unsafe-ffi",
		partId: "developers",
		path: "/docs/book/developers/unsafe-ffi",
		aliases: [
			"/docs/book/developers/rust/unsafe-ffi",
			"/docs/language/developers/rust/unsafe-ffi",
			"/learn/language/developers/rust/unsafe-ffi",
		],
		sourcePath: "docs/what/language/developers/rust/unsafe-ffi.md",
		questions: [],
	},
	{
		id: "rust-testing-tooling",
		partId: "developers",
		path: "/docs/book/developers/testing-tooling",
		aliases: [
			"/docs/book/developers/rust/testing-tooling",
			"/docs/language/developers/rust/testing-tooling",
			"/learn/language/developers/rust/testing-tooling",
		],
		sourcePath: "docs/what/language/developers/rust/testing-tooling.md",
		questions: [],
	},
] satisfies readonly Omit<BookPageDefinition, "sectionId">[];

type BookSectionId = (typeof bookSections)[number]["id"];
type RawBookPageDefinition = (typeof rawBookPages)[number];

function sectionIdForPage(page: RawBookPageDefinition): BookSectionId {
	switch (page.partId) {
		case "start":
			return "start-foundations";
		case "core":
			if (
				page.id === "functions" ||
				page.id === "lambdas" ||
				page.id === "calls" ||
				page.id === "methods"
			) {
				return "core-functions";
			}
			return "core-expressions";
		case "data":
			return "data-modeling";
		case "organization":
			return "organization-modules";
		case "types":
			return "types-foundations";
		case "abstractions":
			return "abstractions-laws";
		case "effects-runtime":
			if (
				page.id === "foundation" ||
				page.id === "runtime" ||
				page.id === "stdlib"
			) {
				return "effects-runtime-model";
			}
			return "effects-handling";
		case "developers":
			if (
				page.id === "musi-for-rust-developers" ||
				page.id.startsWith("rust-")
			) {
				return "developers-rust";
			}
			if (page.id.startsWith("js-ts-")) {
				return "developers-javascript-typescript";
			}
			return "developers-guides";
		case "advanced":
			if (
				page.id === "attributes" ||
				page.id === "foreign" ||
				page.id === "unsafe-and-ffi"
			) {
				return "advanced-interop";
			}
			return "advanced-meta-tooling";
		default:
			throw new Error(`unknown part ${page.partId} for page ${page.id}`);
	}
}

export const bookPages = rawBookPages.map((page) => ({
	...page,
	sectionId: sectionIdForPage(page),
})) satisfies readonly BookPageDefinition[];
