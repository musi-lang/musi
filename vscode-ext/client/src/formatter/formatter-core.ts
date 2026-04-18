import * as path from "node:path";

export const MUSI_FORMAT_COMMAND = "musi.fmt";

const MARKDOWN_EXTENSIONS = new Set([
	".md",
	".markdown",
	".mdown",
	".mdwn",
	".mkd",
	".mkdn",
]);

export type FormatKind = "ms" | "markdown";

export function formatKindForDocument(
	languageId: string,
	filePath: string,
): FormatKind | undefined {
	if (languageId === "musi" || path.extname(filePath).toLowerCase() === ".ms") {
		return "ms";
	}
	if (
		languageId === "markdown" ||
		MARKDOWN_EXTENSIONS.has(path.extname(filePath).toLowerCase())
	) {
		return "markdown";
	}
	return undefined;
}

export function formatArgs(kind: FormatKind): string[] {
	return ["fmt", "--ext", kind, "-"];
}

export function shouldUseCliFormatter(
	kind: FormatKind,
	isLspRunning: boolean,
	forExplicitCommand: boolean,
): boolean {
	return forExplicitCommand || kind === "markdown" || !isLspRunning;
}
