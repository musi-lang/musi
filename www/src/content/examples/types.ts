import type { SnippetEvidence } from "../snippet-registry";
import type { ComparisonLanguage } from "./languages";

export interface ExampleVariant {
	language: ComparisonLanguage;
	sourceText: string;
	evidence: SnippetEvidence;
}

export interface ExampleGroup {
	id: string;
	title: string;
	caption: string;
	note: string;
	defaultLanguage: "musi";
	variants: Record<ComparisonLanguage, ExampleVariant>;
}
