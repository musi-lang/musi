import { spawn } from "node:child_process";
import * as path from "node:path";
import * as vscode from "vscode";
import { findCliPath, showCliNotFoundUI } from "../bootstrap.ts";
import {
	findOwningManifestPathForUri,
	loadPackageRoot,
} from "../manifest/manifest.ts";
import {
	type FormatEditorOptions,
	type FormatKind,
	formatArgs,
	formatKindForDocument,
	shouldUseCliFormatter,
} from "./formatter-core.ts";

interface CliFormatResult {
	readonly stdout: string;
	readonly stderr: string;
	readonly exitCode: number;
}

function fullDocumentRange(document: vscode.TextDocument): vscode.Range {
	return new vscode.Range(
		document.positionAt(0),
		document.positionAt(document.getText().length),
	);
}

function workspaceFolderForDocument(
	document: vscode.TextDocument,
): vscode.WorkspaceFolder | undefined {
	return vscode.workspace.getWorkspaceFolder(document.uri);
}

function activeEditorFormatOptions(
	editor: vscode.TextEditor,
): FormatEditorOptions {
	const options: FormatEditorOptions = {};
	if (typeof editor.options.insertSpaces === "boolean") {
		return {
			...options,
			insertSpaces: editor.options.insertSpaces,
			...(typeof editor.options.tabSize === "number"
				? { tabSize: editor.options.tabSize }
				: {}),
		};
	}
	if (typeof editor.options.tabSize === "number") {
		return { tabSize: editor.options.tabSize };
	}
	return options;
}

async function formatCwd(document: vscode.TextDocument): Promise<string> {
	const manifestPath = findOwningManifestPathForUri(document.uri);
	if (manifestPath) {
		return (await loadPackageRoot(manifestPath)).rootDir;
	}
	const workspaceFolder = workspaceFolderForDocument(document);
	if (workspaceFolder) {
		return workspaceFolder.uri.fsPath;
	}
	return path.dirname(document.uri.fsPath);
}

function runCliFormat(
	document: vscode.TextDocument,
	kind: FormatKind,
	cwd: string,
	options: FormatEditorOptions,
): Promise<CliFormatResult> {
	const cliPath = findCliPath();
	if (!cliPath) {
		return Promise.reject(new Error("Musi CLI binary not found"));
	}
	return new Promise((resolve, reject) => {
		const proc = spawn(cliPath, formatArgs(kind, options), {
			cwd,
			stdio: ["pipe", "pipe", "pipe"],
		});
		let stdout = "";
		let stderr = "";
		proc.stdout.on("data", (chunk: Buffer) => {
			stdout += chunk.toString();
		});
		proc.stderr.on("data", (chunk: Buffer) => {
			stderr += chunk.toString();
		});
		proc.on("error", (error) => {
			reject(error);
		});
		proc.on("close", (code) => {
			resolve({
				stdout,
				stderr,
				exitCode: code ?? 1,
			});
		});
		proc.stdin.end(document.getText());
	});
}

async function formatDocumentWithCli(
	document: vscode.TextDocument,
	forExplicitCommand: boolean,
	isLspRunning: boolean,
	options: FormatEditorOptions = {},
): Promise<vscode.TextEdit[]> {
	if (document.uri.scheme !== "file") {
		return [];
	}
	const kind = formatKindForDocument(document.languageId, document.uri.fsPath);
	if (!kind || !shouldUseCliFormatter(kind, isLspRunning, forExplicitCommand)) {
		return [];
	}
	const cliPath = findCliPath();
	if (!cliPath) {
		await showCliNotFoundUI();
		return [];
	}
	const result = await runCliFormat(
		document,
		kind,
		await formatCwd(document),
		options,
	);
	if (result.exitCode !== 0) {
		const message = result.stderr.trim() || result.stdout.trim();
		throw new Error(
			message || `musi fmt failed with exit code ${result.exitCode}`,
		);
	}
	if (result.stdout === document.getText()) {
		return [];
	}
	return [vscode.TextEdit.replace(fullDocumentRange(document), result.stdout)];
}

export class CliFormatProvider
	implements vscode.DocumentFormattingEditProvider
{
	#formatMusi: boolean;

	constructor(formatMusi: boolean) {
		this.#formatMusi = formatMusi;
	}

	async provideDocumentFormattingEdits(
		document: vscode.TextDocument,
		options: vscode.FormattingOptions,
	): Promise<vscode.TextEdit[]> {
		return formatDocumentWithCli(document, false, !this.#formatMusi, {
			insertSpaces: options.insertSpaces,
			tabSize: options.tabSize,
		});
	}
}

export class FormatterController implements vscode.Disposable {
	#context: vscode.ExtensionContext;
	#musi: vscode.Disposable | undefined;
	#markdown: vscode.Disposable;

	constructor(context: vscode.ExtensionContext) {
		this.#context = context;
		this.#markdown = vscode.languages.registerDocumentFormattingEditProvider(
			{ scheme: "file", language: "markdown" },
			new CliFormatProvider(false),
		);
	}

	setLspRunning(isRunning: boolean) {
		if (isRunning) {
			this.#musi?.dispose();
			this.#musi = undefined;
			return;
		}
		if (this.#musi) {
			return;
		}
		this.#musi = vscode.languages.registerDocumentFormattingEditProvider(
			{ scheme: "file", language: "musi" },
			new CliFormatProvider(true),
		);
		this.#context.subscriptions.push(this.#musi);
	}

	dispose() {
		this.#musi?.dispose();
		this.#markdown.dispose();
	}
}

export async function formatActiveDocumentWithCli() {
	const editor = vscode.window.activeTextEditor;
	if (!editor) {
		await vscode.window.showWarningMessage("No active document to format.");
		return;
	}
	try {
		const edits = await formatDocumentWithCli(
			editor.document,
			true,
			false,
			activeEditorFormatOptions(editor),
		);
		if (edits.length === 0) {
			return;
		}
		const workspaceEdit = new vscode.WorkspaceEdit();
		for (const edit of edits) {
			workspaceEdit.replace(editor.document.uri, edit.range, edit.newText);
		}
		await vscode.workspace.applyEdit(workspaceEdit);
	} catch (error) {
		await vscode.window.showErrorMessage(
			`Musi format failed: ${String(error)}`,
		);
	}
}
