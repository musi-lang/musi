import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

export interface DeveloperComparison {
	id: string;
	sourceLabel: string;
	sourceLanguage: string;
	sourceText: string;
	musiLabel: string;
	musiLanguage: string;
	musiSourceText: string;
	mindset: readonly string[];
	evidence: { path: string; line: number };
}

interface DeveloperProfile {
	label: string;
	language: string;
	mindset: readonly string[];
}

interface DeveloperComparisonSpec {
	id: string;
	profileId: keyof typeof developerProfiles;
	sourcePath: string;
	musiPath: string;
	evidence: { path: string; line: number };
}

const examplesRoot = join(dirname(fileURLToPath(import.meta.url)), "examples");

const readExample = (relativePath: string) =>
	readFileSync(join(examplesRoot, relativePath), "utf8").trimEnd();

export const developerProfiles = {
	c99: {
		label: "C99",
		language: "c",
		mindset: [
			"Treat Musi modules as source boundaries, not header/source pairs.",
			"Use data for shapes and variants; use classes only when a type must supply behavior.",
		],
	},
	cpp17: {
		label: "C++17",
		language: "cpp",
		mindset: [
			"Read Musi classes closer to concepts or traits than C++ classes.",
			"Use records/data for stored shape; use instances for behavior supplied by a type.",
		],
	},
	csharp: {
		label: "C#",
		language: "csharp",
		mindset: [
			"Do not map C# classes directly to Musi classes; Musi classes are behavior contracts.",
			"Use records and data for composite values, then add instances when operations need shared behavior.",
		],
	},
	go: {
		label: "Go",
		language: "go",
		mindset: [
			"Read Musi classes as explicit behavior requirements, closer to interfaces than structs.",
			"Use data and records for values; use instances where Go would lean on method sets.",
		],
	},
	java: {
		label: "Java",
		language: "java",
		mindset: [
			"Do not read Musi class as Java class; it is a typeclass-style behavior contract.",
			"Use records/data for composite values, and instances where Java would use interfaces plus implementations.",
		],
	},
	"javascript-typescript": {
		label: "TypeScript",
		language: "typescript",
		mindset: [
			"Do not read Musi class as a prototype or object constructor; it is a behavior contract.",
			"Use records/data for object-shaped values, and instances for shared typed behavior.",
		],
	},
	lua: {
		label: "Lua",
		language: "lua",
		mindset: [
			"Do not map tables/metatables directly to Musi classes; Musi classes name required behavior.",
			"Use records/data for table-like shape, then instances for operations a type promises.",
		],
	},
	python: {
		label: "Python",
		language: "python",
		mindset: [
			"Do not read Musi class as Python class; it is closer to an explicit protocol with instances.",
			"Use records/data for object shape, and classes/laws for behavior callers rely on.",
		],
	},
	rust: {
		label: "Rust",
		language: "rust",
		mindset: [
			"Read Musi classes closest to Rust traits, with laws documenting behavior contracts.",
			"Use data for variants and records for fields; use instances where trait impl instincts apply.",
		],
	},
} as const satisfies Record<string, DeveloperProfile>;

export const developerComparisonSpecs = [
	{
		id: "c99-arrays-pointers-slices",
		profileId: "c99",
		sourcePath: "c99/c99-arrays-pointers-slices.source.c",
		musiPath: "shared/prices-index.musi.ms",
		evidence: {
			path: "docs/what/language/developers/c99/arrays-pointers-slices.md",
			line: 11,
		},
	},
	{
		id: "c99-blocks-control-flow",
		profileId: "c99",
		sourcePath: "c99/c99-blocks-control-flow.source.c",
		musiPath: "shared/delivery-fee-match.musi.ms",
		evidence: {
			path: "docs/what/language/developers/c99/blocks-control-flow.md",
			line: 11,
		},
	},
	{
		id: "c99-enums-variants-patterns",
		profileId: "c99",
		sourcePath: "c99/c99-enums-variants-patterns.source.c",
		musiPath: "shared/traffic-light-pattern.musi.ms",
		evidence: {
			path: "docs/what/language/developers/c99/enums-variants-patterns.md",
			line: 11,
		},
	},
	{
		id: "c99-errors-results-effects",
		profileId: "c99",
		sourcePath: "c99/c99-errors-results-effects.source.c",
		musiPath: "shared/runtime-time-effect.musi.ms",
		evidence: {
			path: "docs/what/language/developers/c99/errors-results-effects.md",
			line: 11,
		},
	},
	{
		id: "c99-headers-modules-packages",
		profileId: "c99",
		sourcePath: "c99/c99-headers-modules-packages.source.c",
		musiPath: "shared/option-default-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/c99/headers-modules-packages.md",
			line: 11,
		},
	},
	{
		id: "c99-methods-and-receiver-calls",
		profileId: "c99",
		sourcePath: "c99/c99-methods-and-receiver-calls.source.c",
		musiPath: "shared/text-normalize-pipeline.musi.ms",
		evidence: {
			path: "docs/what/language/developers/c99/methods-and-receiver-calls.md",
			line: 11,
		},
	},
	{
		id: "c99-null-option-result",
		profileId: "c99",
		sourcePath: "c99/c99-null-option-result.source.c",
		musiPath: "shared/option-ticket-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/c99/null-option-result.md",
			line: 11,
		},
	},
	{
		id: "c99-structs-classes-records",
		profileId: "c99",
		sourcePath: "c99/c99-structs-classes-records.source.c",
		musiPath: "shared/show-class-instance.musi.ms",
		evidence: {
			path: "docs/what/language/developers/c99/structs-classes-records.md",
			line: 11,
		},
	},
	{
		id: "c99-templates-concepts-classes-laws",
		profileId: "c99",
		sourcePath: "c99/c99-templates-concepts-classes-laws.source.c",
		musiPath: "shared/show-class-instance.musi.ms",
		evidence: {
			path: "docs/what/language/developers/c99/templates-concepts-classes-laws.md",
			line: 11,
		},
	},
	{
		id: "c99-testing-tooling",
		profileId: "c99",
		sourcePath: "c99/c99-testing-tooling.source.c",
		musiPath: "shared/testing-receipt-lines.musi.ms",
		evidence: {
			path: "docs/what/language/developers/c99/testing-tooling.md",
			line: 11,
		},
	},
	{
		id: "c99-unsafe-ffi-native-boundaries",
		profileId: "c99",
		sourcePath: "c99/c99-unsafe-ffi-native-boundaries.source.c",
		musiPath: "shared/ffi-clock.musi.ms",
		evidence: {
			path: "docs/what/language/developers/c99/unsafe-ffi-native-boundaries.md",
			line: 11,
		},
	},
	{
		id: "c99-values-functions-expressions",
		profileId: "c99",
		sourcePath: "c99/c99-values-functions-expressions.source.c",
		musiPath: "shared/receipt-total-function.musi.ms",
		evidence: {
			path: "docs/what/language/developers/c99/values-functions-expressions.md",
			line: 11,
		},
	},
	{
		id: "c99-variables-mutation",
		profileId: "c99",
		sourcePath: "c99/c99-variables-mutation.source.c",
		musiPath: "shared/queue-depth-mutation.musi.ms",
		evidence: {
			path: "docs/what/language/developers/c99/variables-mutation.md",
			line: 11,
		},
	},
	{
		id: "cpp17-arrays-pointers-slices",
		profileId: "cpp17",
		sourcePath: "cpp17/cpp17-arrays-pointers-slices.source.cpp",
		musiPath: "shared/prices-index.musi.ms",
		evidence: {
			path: "docs/what/language/developers/cpp17/arrays-pointers-slices.md",
			line: 11,
		},
	},
	{
		id: "cpp17-blocks-control-flow",
		profileId: "cpp17",
		sourcePath: "cpp17/cpp17-blocks-control-flow.source.cpp",
		musiPath: "shared/delivery-fee-match.musi.ms",
		evidence: {
			path: "docs/what/language/developers/cpp17/blocks-control-flow.md",
			line: 11,
		},
	},
	{
		id: "cpp17-enums-variants-patterns",
		profileId: "cpp17",
		sourcePath: "cpp17/cpp17-enums-variants-patterns.source.cpp",
		musiPath: "shared/traffic-light-pattern.musi.ms",
		evidence: {
			path: "docs/what/language/developers/cpp17/enums-variants-patterns.md",
			line: 11,
		},
	},
	{
		id: "cpp17-errors-results-effects",
		profileId: "cpp17",
		sourcePath: "cpp17/cpp17-errors-results-effects.source.cpp",
		musiPath: "shared/runtime-time-effect.musi.ms",
		evidence: {
			path: "docs/what/language/developers/cpp17/errors-results-effects.md",
			line: 11,
		},
	},
	{
		id: "cpp17-headers-modules-packages",
		profileId: "cpp17",
		sourcePath: "cpp17/cpp17-headers-modules-packages.source.cpp",
		musiPath: "shared/option-default-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/cpp17/headers-modules-packages.md",
			line: 11,
		},
	},
	{
		id: "cpp17-methods-and-receiver-calls",
		profileId: "cpp17",
		sourcePath: "cpp17/cpp17-methods-and-receiver-calls.source.cpp",
		musiPath: "shared/text-normalize-pipeline.musi.ms",
		evidence: {
			path: "docs/what/language/developers/cpp17/methods-and-receiver-calls.md",
			line: 11,
		},
	},
	{
		id: "cpp17-null-option-result",
		profileId: "cpp17",
		sourcePath: "cpp17/cpp17-null-option-result.source.cpp",
		musiPath: "shared/option-ticket-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/cpp17/null-option-result.md",
			line: 11,
		},
	},
	{
		id: "cpp17-structs-classes-records",
		profileId: "cpp17",
		sourcePath: "cpp17/cpp17-structs-classes-records.source.cpp",
		musiPath: "shared/show-class-instance.musi.ms",
		evidence: {
			path: "docs/what/language/developers/cpp17/structs-classes-records.md",
			line: 11,
		},
	},
	{
		id: "cpp17-templates-concepts-classes-laws",
		profileId: "cpp17",
		sourcePath: "cpp17/cpp17-templates-concepts-classes-laws.source.cpp",
		musiPath: "shared/show-class-instance.musi.ms",
		evidence: {
			path: "docs/what/language/developers/cpp17/templates-concepts-classes-laws.md",
			line: 11,
		},
	},
	{
		id: "cpp17-testing-tooling",
		profileId: "cpp17",
		sourcePath: "cpp17/cpp17-testing-tooling.source.cpp",
		musiPath: "shared/testing-receipt-lines.musi.ms",
		evidence: {
			path: "docs/what/language/developers/cpp17/testing-tooling.md",
			line: 11,
		},
	},
	{
		id: "cpp17-unsafe-ffi-native-boundaries",
		profileId: "cpp17",
		sourcePath: "cpp17/cpp17-unsafe-ffi-native-boundaries.source.cpp",
		musiPath: "shared/ffi-clock.musi.ms",
		evidence: {
			path: "docs/what/language/developers/cpp17/unsafe-ffi-native-boundaries.md",
			line: 11,
		},
	},
	{
		id: "cpp17-values-functions-expressions",
		profileId: "cpp17",
		sourcePath: "cpp17/cpp17-values-functions-expressions.source.cpp",
		musiPath: "shared/receipt-total-function.musi.ms",
		evidence: {
			path: "docs/what/language/developers/cpp17/values-functions-expressions.md",
			line: 11,
		},
	},
	{
		id: "cpp17-variables-mutation",
		profileId: "cpp17",
		sourcePath: "cpp17/cpp17-variables-mutation.source.cpp",
		musiPath: "shared/queue-depth-mutation.musi.ms",
		evidence: {
			path: "docs/what/language/developers/cpp17/variables-mutation.md",
			line: 11,
		},
	},
	{
		id: "csharp-blocks-control-flow",
		profileId: "csharp",
		sourcePath: "csharp/csharp-blocks-control-flow.source.cs",
		musiPath: "shared/delivery-fee-match.musi.ms",
		evidence: {
			path: "docs/what/language/developers/csharp/blocks-control-flow.md",
			line: 11,
		},
	},
	{
		id: "csharp-collections-linq-pipelines",
		profileId: "csharp",
		sourcePath: "csharp/csharp-collections-linq-pipelines.source.cs",
		musiPath: "shared/prices-index.musi.ms",
		evidence: {
			path: "docs/what/language/developers/csharp/collections-linq-pipelines.md",
			line: 11,
		},
	},
	{
		id: "csharp-exceptions-effects",
		profileId: "csharp",
		sourcePath: "csharp/csharp-exceptions-effects.source.cs",
		musiPath: "shared/runtime-time-effect.musi.ms",
		evidence: {
			path: "docs/what/language/developers/csharp/exceptions-effects.md",
			line: 11,
		},
	},
	{
		id: "csharp-extension-methods-and-calls",
		profileId: "csharp",
		sourcePath: "csharp/csharp-extension-methods-and-calls.source.cs",
		musiPath: "shared/text-normalize-pipeline.musi.ms",
		evidence: {
			path: "docs/what/language/developers/csharp/extension-methods-and-calls.md",
			line: 11,
		},
	},
	{
		id: "csharp-generics-interfaces-laws",
		profileId: "csharp",
		sourcePath: "csharp/csharp-generics-interfaces-laws.source.cs",
		musiPath: "shared/show-class-instance.musi.ms",
		evidence: {
			path: "docs/what/language/developers/csharp/generics-interfaces-laws.md",
			line: 11,
		},
	},
	{
		id: "csharp-namespaces-modules-packages",
		profileId: "csharp",
		sourcePath: "csharp/csharp-namespaces-modules-packages.source.cs",
		musiPath: "shared/option-default-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/csharp/namespaces-modules-packages.md",
			line: 11,
		},
	},
	{
		id: "csharp-null-option-result",
		profileId: "csharp",
		sourcePath: "csharp/csharp-null-option-result.source.cs",
		musiPath: "shared/option-ticket-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/csharp/null-option-result.md",
			line: 11,
		},
	},
	{
		id: "csharp-records-classes-objects",
		profileId: "csharp",
		sourcePath: "csharp/csharp-records-classes-objects.source.cs",
		musiPath: "shared/show-class-instance.musi.ms",
		evidence: {
			path: "docs/what/language/developers/csharp/records-classes-objects.md",
			line: 11,
		},
	},
	{
		id: "csharp-testing-tooling",
		profileId: "csharp",
		sourcePath: "csharp/csharp-testing-tooling.source.cs",
		musiPath: "shared/testing-receipt-lines.musi.ms",
		evidence: {
			path: "docs/what/language/developers/csharp/testing-tooling.md",
			line: 11,
		},
	},
	{
		id: "csharp-unions-pattern-matching",
		profileId: "csharp",
		sourcePath: "csharp/csharp-unions-pattern-matching.source.cs",
		musiPath: "shared/traffic-light-pattern.musi.ms",
		evidence: {
			path: "docs/what/language/developers/csharp/unions-pattern-matching.md",
			line: 11,
		},
	},
	{
		id: "csharp-unsafe-interop-ffi",
		profileId: "csharp",
		sourcePath: "csharp/csharp-unsafe-interop-ffi.source.cs",
		musiPath: "shared/ffi-clock.musi.ms",
		evidence: {
			path: "docs/what/language/developers/csharp/unsafe-interop-ffi.md",
			line: 11,
		},
	},
	{
		id: "csharp-values-methods-expressions",
		profileId: "csharp",
		sourcePath: "csharp/csharp-values-methods-expressions.source.cs",
		musiPath: "shared/text-normalize-pipeline.musi.ms",
		evidence: {
			path: "docs/what/language/developers/csharp/values-methods-expressions.md",
			line: 11,
		},
	},
	{
		id: "csharp-variables-mutation",
		profileId: "csharp",
		sourcePath: "csharp/csharp-variables-mutation.source.cs",
		musiPath: "shared/queue-depth-mutation.musi.ms",
		evidence: {
			path: "docs/what/language/developers/csharp/variables-mutation.md",
			line: 11,
		},
	},
	{
		id: "go-blocks-branching-loops",
		profileId: "go",
		sourcePath: "go/go-blocks-branching-loops.source.go",
		musiPath: "shared/delivery-fee-match.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/blocks-branching-loops.md",
			line: 11,
		},
	},
	{
		id: "go-errors-results-effects",
		profileId: "go",
		sourcePath: "go/go-errors-results-effects.source.go",
		musiPath: "shared/runtime-time-effect.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/errors-results-effects.md",
			line: 11,
		},
	},
	{
		id: "go-generics-type-constructors",
		profileId: "go",
		sourcePath: "go/go-generics-type-constructors.source.go",
		musiPath: "shared/box-generic.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/generics-type-constructors.md",
			line: 11,
		},
	},
	{
		id: "go-goroutines-channels-context-effects",
		profileId: "go",
		sourcePath: "go/go-goroutines-channels-context-effects.source.go",
		musiPath: "shared/runtime-time-effect.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/goroutines-channels-context-effects.md",
			line: 11,
		},
	},
	{
		id: "go-interfaces-classes-laws",
		profileId: "go",
		sourcePath: "go/go-interfaces-classes-laws.source.go",
		musiPath: "shared/show-class-instance.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/interfaces-classes-laws.md",
			line: 11,
		},
	},
	{
		id: "go-methods-receiver-calls",
		profileId: "go",
		sourcePath: "go/go-methods-receiver-calls.source.go",
		musiPath: "shared/text-normalize-pipeline.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/methods-receiver-calls.md",
			line: 11,
		},
	},
	{
		id: "go-nil-option-result",
		profileId: "go",
		sourcePath: "go/go-nil-option-result.source.go",
		musiPath: "shared/option-ticket-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/nil-option-result.md",
			line: 11,
		},
	},
	{
		id: "go-packages-imports-exports",
		profileId: "go",
		sourcePath: "go/go-packages-imports-exports.source.go",
		musiPath: "shared/option-default-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/packages-imports-exports.md",
			line: 11,
		},
	},
	{
		id: "go-slices-arrays-maps-pipelines",
		profileId: "go",
		sourcePath: "go/go-slices-arrays-maps-pipelines.source.go",
		musiPath: "shared/prices-index.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/slices-arrays-maps-pipelines.md",
			line: 11,
		},
	},
	{
		id: "go-structs-records-field-updates",
		profileId: "go",
		sourcePath: "go/go-structs-records-field-updates.source.go",
		musiPath: "shared/pet-record-field.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/structs-records-field-updates.md",
			line: 11,
		},
	},
	{
		id: "go-testing-tooling",
		profileId: "go",
		sourcePath: "go/go-testing-tooling.source.go",
		musiPath: "shared/testing-receipt-lines.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/testing-tooling.md",
			line: 11,
		},
	},
	{
		id: "go-unsafe-cgo-ffi",
		profileId: "go",
		sourcePath: "go/go-unsafe-cgo-ffi.source.go",
		musiPath: "shared/ffi-clock.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/unsafe-cgo-ffi.md",
			line: 11,
		},
	},
	{
		id: "go-values-functions-expressions",
		profileId: "go",
		sourcePath: "go/go-values-functions-expressions.source.go",
		musiPath: "shared/receipt-total-function.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/values-functions-expressions.md",
			line: 11,
		},
	},
	{
		id: "go-variables-short-declarations-mutation",
		profileId: "go",
		sourcePath: "go/go-variables-short-declarations-mutation.source.go",
		musiPath: "shared/queue-depth-mutation.musi.ms",
		evidence: {
			path: "docs/what/language/developers/go/variables-short-declarations-mutation.md",
			line: 11,
		},
	},
	{
		id: "java-blocks-control-flow",
		profileId: "java",
		sourcePath: "java/java-blocks-control-flow.source.java",
		musiPath: "shared/delivery-fee-match.musi.ms",
		evidence: {
			path: "docs/what/language/developers/java/blocks-control-flow.md",
			line: 11,
		},
	},
	{
		id: "java-collections-streams-pipelines",
		profileId: "java",
		sourcePath: "java/java-collections-streams-pipelines.source.java",
		musiPath: "shared/prices-index.musi.ms",
		evidence: {
			path: "docs/what/language/developers/java/collections-streams-pipelines.md",
			line: 11,
		},
	},
	{
		id: "java-exceptions-effects",
		profileId: "java",
		sourcePath: "java/java-exceptions-effects.source.java",
		musiPath: "shared/runtime-time-effect.musi.ms",
		evidence: {
			path: "docs/what/language/developers/java/exceptions-effects.md",
			line: 11,
		},
	},
	{
		id: "java-generics-interfaces-laws",
		profileId: "java",
		sourcePath: "java/java-generics-interfaces-laws.source.java",
		musiPath: "shared/show-class-instance.musi.ms",
		evidence: {
			path: "docs/what/language/developers/java/generics-interfaces-laws.md",
			line: 11,
		},
	},
	{
		id: "java-methods-and-receiver-calls",
		profileId: "java",
		sourcePath: "java/java-methods-and-receiver-calls.source.java",
		musiPath: "shared/text-normalize-pipeline.musi.ms",
		evidence: {
			path: "docs/what/language/developers/java/methods-and-receiver-calls.md",
			line: 11,
		},
	},
	{
		id: "java-native-unsafe-ffi",
		profileId: "java",
		sourcePath: "java/java-native-unsafe-ffi.source.java",
		musiPath: "shared/ffi-clock.musi.ms",
		evidence: {
			path: "docs/what/language/developers/java/native-unsafe-ffi.md",
			line: 11,
		},
	},
	{
		id: "java-null-option-result",
		profileId: "java",
		sourcePath: "java/java-null-option-result.source.java",
		musiPath: "shared/option-ticket-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/java/null-option-result.md",
			line: 11,
		},
	},
	{
		id: "java-packages-modules",
		profileId: "java",
		sourcePath: "java/java-packages-modules.source.java",
		musiPath: "shared/option-default-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/java/packages-modules.md",
			line: 11,
		},
	},
	{
		id: "java-records-classes-objects",
		profileId: "java",
		sourcePath: "java/java-records-classes-objects.source.java",
		musiPath: "shared/show-class-instance.musi.ms",
		evidence: {
			path: "docs/what/language/developers/java/records-classes-objects.md",
			line: 11,
		},
	},
	{
		id: "java-sealed-types-patterns",
		profileId: "java",
		sourcePath: "java/java-sealed-types-patterns.source.java",
		musiPath: "shared/traffic-light-pattern.musi.ms",
		evidence: {
			path: "docs/what/language/developers/java/sealed-types-patterns.md",
			line: 11,
		},
	},
	{
		id: "java-testing-tooling",
		profileId: "java",
		sourcePath: "java/java-testing-tooling.source.java",
		musiPath: "shared/testing-receipt-lines.musi.ms",
		evidence: {
			path: "docs/what/language/developers/java/testing-tooling.md",
			line: 11,
		},
	},
	{
		id: "java-values-methods-expressions",
		profileId: "java",
		sourcePath: "java/java-values-methods-expressions.source.java",
		musiPath: "shared/text-normalize-pipeline.musi.ms",
		evidence: {
			path: "docs/what/language/developers/java/values-methods-expressions.md",
			line: 11,
		},
	},
	{
		id: "java-variables-mutation",
		profileId: "java",
		sourcePath: "java/java-variables-mutation.source.java",
		musiPath: "shared/queue-depth-mutation.musi.ms",
		evidence: {
			path: "docs/what/language/developers/java/variables-mutation.md",
			line: 11,
		},
	},
	{
		id: "javascript-typescript-arrays-pipelines",
		profileId: "javascript-typescript",
		sourcePath:
			"javascript-typescript/javascript-typescript-arrays-pipelines.source.txt",
		musiPath: "shared/traffic-light-pattern.musi.ms",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/arrays-pipelines.md",
			line: 11,
		},
	},
	{
		id: "javascript-typescript-classes-behavior",
		profileId: "javascript-typescript",
		sourcePath:
			"javascript-typescript/javascript-typescript-classes-behavior.source.txt",
		musiPath: "shared/show-class-instance.musi.ms",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/classes-behavior.md",
			line: 11,
		},
	},
	{
		id: "javascript-typescript-generics",
		profileId: "javascript-typescript",
		sourcePath:
			"javascript-typescript/javascript-typescript-generics.source.txt",
		musiPath: "shared/box-generic.musi.ms",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/generics.md",
			line: 11,
		},
	},
	{
		id: "javascript-typescript-modules-packages",
		profileId: "javascript-typescript",
		sourcePath:
			"javascript-typescript/javascript-typescript-modules-packages.source.txt",
		musiPath: "shared/option-default-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/modules-packages.md",
			line: 11,
		},
	},
	{
		id: "javascript-typescript-null-result",
		profileId: "javascript-typescript",
		sourcePath:
			"javascript-typescript/javascript-typescript-null-result.source.txt",
		musiPath: "shared/option-ticket-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/null-result.md",
			line: 11,
		},
	},
	{
		id: "javascript-typescript-objects-records",
		profileId: "javascript-typescript",
		sourcePath:
			"javascript-typescript/javascript-typescript-objects-records.source.txt",
		musiPath: "shared/pet-record-field.musi.ms",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/objects-records.md",
			line: 11,
		},
	},
	{
		id: "javascript-typescript-promises-effects",
		profileId: "javascript-typescript",
		sourcePath:
			"javascript-typescript/javascript-typescript-promises-effects.source.txt",
		musiPath: "shared/ffi-clock.musi.ms",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/promises-effects.md",
			line: 11,
		},
	},
	{
		id: "javascript-typescript-state",
		profileId: "javascript-typescript",
		sourcePath: "javascript-typescript/javascript-typescript-state.source.txt",
		musiPath: "shared/queue-depth-mutation.musi.ms",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/state.md",
			line: 11,
		},
	},
	{
		id: "javascript-typescript-testing-tooling",
		profileId: "javascript-typescript",
		sourcePath:
			"javascript-typescript/javascript-typescript-testing-tooling.source.txt",
		musiPath: "shared/testing-receipt-lines.musi.ms",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/testing-tooling.md",
			line: 11,
		},
	},
	{
		id: "javascript-typescript-unions-variants",
		profileId: "javascript-typescript",
		sourcePath:
			"javascript-typescript/javascript-typescript-unions-variants.source.txt",
		musiPath: "shared/traffic-light-pattern.musi.ms",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/unions-variants.md",
			line: 11,
		},
	},
	{
		id: "javascript-typescript-values-functions",
		profileId: "javascript-typescript",
		sourcePath:
			"javascript-typescript/javascript-typescript-values-functions.source.txt",
		musiPath: "shared/receipt-total-function.musi.ms",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/values-functions.md",
			line: 11,
		},
	},
	{
		id: "lua-arrays-sequences-maps-pipelines",
		profileId: "lua",
		sourcePath: "lua/lua-arrays-sequences-maps-pipelines.source.lua",
		musiPath: "shared/prices-index.musi.ms",
		evidence: {
			path: "docs/what/language/developers/lua/arrays-sequences-maps-pipelines.md",
			line: 11,
		},
	},
	{
		id: "lua-blocks-branching-repetition",
		profileId: "lua",
		sourcePath: "lua/lua-blocks-branching-repetition.source.lua",
		musiPath: "shared/delivery-fee-match.musi.ms",
		evidence: {
			path: "docs/what/language/developers/lua/blocks-branching-repetition.md",
			line: 11,
		},
	},
	{
		id: "lua-coroutines-effect-boundaries",
		profileId: "lua",
		sourcePath: "lua/lua-coroutines-effect-boundaries.source.lua",
		musiPath: "shared/ffi-clock.musi.ms",
		evidence: {
			path: "docs/what/language/developers/lua/coroutines-effect-boundaries.md",
			line: 11,
		},
	},
	{
		id: "lua-embedding-c-api-ffi",
		profileId: "lua",
		sourcePath: "lua/lua-embedding-c-api-ffi.source.lua",
		musiPath: "shared/ffi-clock.musi.ms",
		evidence: {
			path: "docs/what/language/developers/lua/embedding-c-api-ffi.md",
			line: 11,
		},
	},
	{
		id: "lua-errors-pcall-effects",
		profileId: "lua",
		sourcePath: "lua/lua-errors-pcall-effects.source.lua",
		musiPath: "shared/runtime-time-effect.musi.ms",
		evidence: {
			path: "docs/what/language/developers/lua/errors-pcall-effects.md",
			line: 11,
		},
	},
	{
		id: "lua-functions-closures-named-calls",
		profileId: "lua",
		sourcePath: "lua/lua-functions-closures-named-calls.source.lua",
		musiPath: "shared/text-normalize-pipeline.musi.ms",
		evidence: {
			path: "docs/what/language/developers/lua/functions-closures-named-calls.md",
			line: 11,
		},
	},
	{
		id: "lua-metatables-classes-instances-laws",
		profileId: "lua",
		sourcePath: "lua/lua-metatables-classes-instances-laws.source.lua",
		musiPath: "shared/show-class-instance.musi.ms",
		evidence: {
			path: "docs/what/language/developers/lua/metatables-classes-instances-laws.md",
			line: 11,
		},
	},
	{
		id: "lua-modules-imports-exports",
		profileId: "lua",
		sourcePath: "lua/lua-modules-imports-exports.source.lua",
		musiPath: "shared/option-default-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/lua/modules-imports-exports.md",
			line: 11,
		},
	},
	{
		id: "lua-nil-option-result",
		profileId: "lua",
		sourcePath: "lua/lua-nil-option-result.source.lua",
		musiPath: "shared/option-ticket-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/lua/nil-option-result.md",
			line: 11,
		},
	},
	{
		id: "lua-pattern-matching-data-variants",
		profileId: "lua",
		sourcePath: "lua/lua-pattern-matching-data-variants.source.lua",
		musiPath: "shared/traffic-light-pattern.musi.ms",
		evidence: {
			path: "docs/what/language/developers/lua/pattern-matching-data-variants.md",
			line: 11,
		},
	},
	{
		id: "lua-tables-records-field-updates",
		profileId: "lua",
		sourcePath: "lua/lua-tables-records-field-updates.source.lua",
		musiPath: "shared/pet-record-field.musi.ms",
		evidence: {
			path: "docs/what/language/developers/lua/tables-records-field-updates.md",
			line: 11,
		},
	},
	{
		id: "lua-testing-tooling",
		profileId: "lua",
		sourcePath: "lua/lua-testing-tooling.source.lua",
		musiPath: "shared/testing-receipt-lines.musi.ms",
		evidence: {
			path: "docs/what/language/developers/lua/testing-tooling.md",
			line: 11,
		},
	},
	{
		id: "lua-values-locals-expressions",
		profileId: "lua",
		sourcePath: "lua/lua-values-locals-expressions.source.lua",
		musiPath: "shared/receipt-total-function.musi.ms",
		evidence: {
			path: "docs/what/language/developers/lua/values-locals-expressions.md",
			line: 11,
		},
	},
	{
		id: "python-blocks-control-flow",
		profileId: "python",
		sourcePath: "python/python-blocks-control-flow.source.py",
		musiPath: "shared/delivery-fee-match.musi.ms",
		evidence: {
			path: "docs/what/language/developers/python/blocks-control-flow.md",
			line: 11,
		},
	},
	{
		id: "python-collections-pipelines",
		profileId: "python",
		sourcePath: "python/python-collections-pipelines.source.py",
		musiPath: "shared/prices-index.musi.ms",
		evidence: {
			path: "docs/what/language/developers/python/collections-pipelines.md",
			line: 11,
		},
	},
	{
		id: "python-data-variants-patterns",
		profileId: "python",
		sourcePath: "python/python-data-variants-patterns.source.py",
		musiPath: "shared/traffic-light-pattern.musi.ms",
		evidence: {
			path: "docs/what/language/developers/python/data-variants-patterns.md",
			line: 11,
		},
	},
	{
		id: "python-exceptions-effects",
		profileId: "python",
		sourcePath: "python/python-exceptions-effects.source.py",
		musiPath: "shared/runtime-time-effect.musi.ms",
		evidence: {
			path: "docs/what/language/developers/python/exceptions-effects.md",
			line: 11,
		},
	},
	{
		id: "python-modules-packages",
		profileId: "python",
		sourcePath: "python/python-modules-packages.source.py",
		musiPath: "shared/option-default-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/python/modules-packages.md",
			line: 11,
		},
	},
	{
		id: "python-names-mutation",
		profileId: "python",
		sourcePath: "python/python-names-mutation.source.py",
		musiPath: "shared/queue-depth-mutation.musi.ms",
		evidence: {
			path: "docs/what/language/developers/python/names-mutation.md",
			line: 11,
		},
	},
	{
		id: "python-native-unsafe-ffi",
		profileId: "python",
		sourcePath: "python/python-native-unsafe-ffi.source.py",
		musiPath: "shared/ffi-clock.musi.ms",
		evidence: {
			path: "docs/what/language/developers/python/native-unsafe-ffi.md",
			line: 11,
		},
	},
	{
		id: "python-none-option-result",
		profileId: "python",
		sourcePath: "python/python-none-option-result.source.py",
		musiPath: "shared/option-ticket-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/python/none-option-result.md",
			line: 11,
		},
	},
	{
		id: "python-records-objects-dicts",
		profileId: "python",
		sourcePath: "python/python-records-objects-dicts.source.py",
		musiPath: "shared/pet-record-field.musi.ms",
		evidence: {
			path: "docs/what/language/developers/python/records-objects-dicts.md",
			line: 11,
		},
	},
	{
		id: "python-testing-tooling",
		profileId: "python",
		sourcePath: "python/python-testing-tooling.source.py",
		musiPath: "shared/testing-receipt-lines.musi.ms",
		evidence: {
			path: "docs/what/language/developers/python/testing-tooling.md",
			line: 11,
		},
	},
	{
		id: "python-types-generics-protocols",
		profileId: "python",
		sourcePath: "python/python-types-generics-protocols.source.py",
		musiPath: "shared/box-generic.musi.ms",
		evidence: {
			path: "docs/what/language/developers/python/types-generics-protocols.md",
			line: 11,
		},
	},
	{
		id: "python-values-functions",
		profileId: "python",
		sourcePath: "python/python-values-functions.source.py",
		musiPath: "shared/receipt-total-function.musi.ms",
		evidence: {
			path: "docs/what/language/developers/python/values-functions.md",
			line: 11,
		},
	},
	{
		id: "rust-enums-data",
		profileId: "rust",
		sourcePath: "rust/rust-enums-data.source.rs",
		musiPath: "shared/traffic-light-pattern.musi.ms",
		evidence: {
			path: "docs/what/language/developers/rust/enums-data.md",
			line: 11,
		},
	},
	{
		id: "rust-generics",
		profileId: "rust",
		sourcePath: "rust/rust-generics.source.rs",
		musiPath: "shared/box-generic.musi.ms",
		evidence: {
			path: "docs/what/language/developers/rust/generics.md",
			line: 11,
		},
	},
	{
		id: "rust-modules-packages",
		profileId: "rust",
		sourcePath: "rust/rust-modules-packages.source.rs",
		musiPath: "shared/option-default-gate.musi.ms",
		evidence: {
			path: "docs/what/language/developers/rust/modules-packages.md",
			line: 11,
		},
	},
	{
		id: "rust-mutation",
		profileId: "rust",
		sourcePath: "rust/rust-mutation.source.rs",
		musiPath: "shared/queue-depth-mutation.musi.ms",
		evidence: {
			path: "docs/what/language/developers/rust/mutation.md",
			line: 11,
		},
	},
	{
		id: "rust-records-structs",
		profileId: "rust",
		sourcePath: "rust/rust-records-structs.source.rs",
		musiPath: "shared/pet-record-field.musi.ms",
		evidence: {
			path: "docs/what/language/developers/rust/records-structs.md",
			line: 11,
		},
	},
	{
		id: "rust-results-effects",
		profileId: "rust",
		sourcePath: "rust/rust-results-effects.source.rs",
		musiPath: "shared/runtime-time-effect.musi.ms",
		evidence: {
			path: "docs/what/language/developers/rust/results-effects.md",
			line: 11,
		},
	},
	{
		id: "rust-testing-tooling",
		profileId: "rust",
		sourcePath: "rust/rust-testing-tooling.source.rs",
		musiPath: "shared/testing-receipt-lines.musi.ms",
		evidence: {
			path: "docs/what/language/developers/rust/testing-tooling.md",
			line: 11,
		},
	},
	{
		id: "rust-traits-classes-laws",
		profileId: "rust",
		sourcePath: "rust/rust-traits-classes-laws.source.rs",
		musiPath: "shared/show-class-instance.musi.ms",
		evidence: {
			path: "docs/what/language/developers/rust/traits-classes-laws.md",
			line: 11,
		},
	},
	{
		id: "rust-unsafe-ffi",
		profileId: "rust",
		sourcePath: "rust/rust-unsafe-ffi.source.rs",
		musiPath: "shared/ffi-clock.musi.ms",
		evidence: {
			path: "docs/what/language/developers/rust/unsafe-ffi.md",
			line: 11,
		},
	},
	{
		id: "rust-values-functions",
		profileId: "rust",
		sourcePath: "rust/rust-values-functions.source.rs",
		musiPath: "shared/receipt-total-function.musi.ms",
		evidence: {
			path: "docs/what/language/developers/rust/values-functions.md",
			line: 11,
		},
	},
] as const satisfies readonly DeveloperComparisonSpec[];

export const developerComparisons = developerComparisonSpecs.map(
	(spec): DeveloperComparison => {
		const profile = developerProfiles[spec.profileId];
		return {
			id: spec.id,
			sourceLabel: profile.label,
			sourceLanguage: profile.language,
			sourceText: readExample(spec.sourcePath),
			musiLabel: "Musi",
			musiLanguage: "musi",
			musiSourceText: readExample(spec.musiPath),
			mindset: profile.mindset,
			evidence: spec.evidence,
		};
	},
);

export function findDeveloperComparison(id: string) {
	return developerComparisons.find((comparison) => comparison.id === id);
}
