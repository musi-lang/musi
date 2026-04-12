export const comparisonLanguages = [
	"c",
	"cpp",
	"csharp",
	"go",
	"java",
	"javascript",
	"musi",
	"python",
	"rust",
	"typescript",
] as const;

export type ComparisonLanguage = (typeof comparisonLanguages)[number];

export const comparisonLanguageLabels: Record<ComparisonLanguage, string> = {
	c: "C",
	cpp: "C++",
	csharp: "C#",
	go: "Go",
	java: "Java",
	javascript: "JavaScript",
	musi: "Musi",
	python: "Python",
	rust: "Rust",
	typescript: "TypeScript",
};
