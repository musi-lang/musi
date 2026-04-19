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

export interface FormatEditorOptions {
	readonly insertSpaces?: boolean;
	readonly tabSize?: number;
}

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

export function formatArgs(
	kind: FormatKind,
	options: FormatEditorOptions = {},
): string[] {
	const args = ["fmt", "--ext", kind];
	if (typeof options.tabSize === "number" && Number.isFinite(options.tabSize)) {
		args.push(
			"--indent-width",
			String(Math.max(1, Math.trunc(options.tabSize))),
		);
	}
	if (options.insertSpaces === true) {
		args.push("--use-spaces");
	} else if (options.insertSpaces === false) {
		args.push("--use-tabs");
	}
	args.push("-");
	return args;
}

export function shouldUseCliFormatter(
	kind: FormatKind,
	isLspRunning: boolean,
	forExplicitCommand: boolean,
): boolean {
	return forExplicitCommand || kind === "markdown" || !isLspRunning;
}
