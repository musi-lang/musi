import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import { getConfig } from "./config";

/**
 * Find `musi_lsp` server binary.
 * Search order:
 * 1. User-configured path (`musi.server.path`)
 * 2. Workspace `target/debug/musi_lsp`
 * 3. Workspace `target/release/musi_lsp`
 * 4. Global installation paths
 */
export async function findServerPath(): Promise<string | undefined> {
	const config = getConfig();
	if (config.serverPath) {
		if (fs.existsSync(config.serverPath)) {
			return config.serverPath;
		}
		vscode.window.showWarningMessage(
			`Configured server path not found: ${config.serverPath}`,
		);
	}
	const workspacePath = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
	if (workspacePath) {
		const candidates = [
			path.join(workspacePath, "target", "debug", "musi_lsp"),
			path.join(workspacePath, "target", "release", "musi_lsp"),
		];
		for (const candidate of candidates) {
			if (fs.existsSync(candidate)) {
				return candidate;
			}
		}
	}
	const globalCandidates = [
		"/usr/local/bin/musi_lsp",
		"/usr/bin/musi_lsp",
		path.join(process.env["HOME"] || "", ".cargo", "bin", "musi_lsp"),
	];
	for (const candidate of globalCandidates) {
		if (fs.existsSync(candidate)) {
			return candidate;
		}
	}

	return undefined;
}

export async function validateServer(serverPath: string): Promise<boolean> {
	try {
		fs.accessSync(serverPath, fs.constants.X_OK);
		return true;
	} catch {
		return false;
	}
}

export async function showServerNotFoundUI() {
	const action = await vscode.window.showErrorMessage(
		"Musi LSP server not found. Please run 'cargo build -p musi_lsp' to build server.",
		"Open Terminal",
		"Show Build Instructions",
	);

	if (action === "Open Terminal") {
		const terminal = vscode.window.createTerminal("Musi Build");
		terminal.sendText("cargo build -p musi_lsp");
		terminal.show();
	} else if (action === "Show Build Instructions") {
		vscode.env.openExternal(
			vscode.Uri.parse("https://github.com/musi-lang/musi#Installation"),
		);
	}
}
