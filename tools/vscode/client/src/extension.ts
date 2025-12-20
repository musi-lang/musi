import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import { LanguageClient, TransportKind } from "vscode-languageclient/node";

let client: LanguageClient | undefined;
let statusBarItem: vscode.StatusBarItem;

function updateStatus(message: string, state: "loading" | "ready" | "error") {
	if (!statusBarItem) {
		statusBarItem = vscode.window.createStatusBarItem(
			vscode.StatusBarAlignment.Right,
			100,
		);
		statusBarItem.command = "musi.showLogs";
	}

	statusBarItem.text = `$(play) Musi LSP: ${message}`;

	switch (state) {
		case "loading":
			statusBarItem.color = new vscode.ThemeColor(
				"statusBarItem.warningForeground",
			);
			break;
		case "ready":
			statusBarItem.color = new vscode.ThemeColor(
				"statusBarItem.prominentForeground",
			);
			break;
		case "error":
			statusBarItem.color = new vscode.ThemeColor("errorForeground");
			break;
	}

	statusBarItem.show();
}

async function findServerPath(): Promise<string | undefined> {
	const workspacePath = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
	if (!workspacePath) {
		return undefined;
	}

	const candidates = [
		path.join(
			workspacePath,
			"_build",
			"default",
			"tools",
			"lsp",
			"bin",
			"main.exe",
		),
		path.join(
			workspacePath,
			"_build",
			"default",
			"tools",
			"lsp",
			"bin",
			"main",
		),
		path.join(workspacePath, "_build", "install", "default", "bin", "main.exe"),
		path.join(workspacePath, "_build", "install", "default", "bin", "main"),
	];

	for (const candidate of candidates) {
		if (fs.existsSync(candidate)) {
			return candidate;
		}
	}

	return undefined;
}

async function startClientWithTimeout(serverPath: string): Promise<void> {
	const serverOptions = {
		run: { command: serverPath, transport: TransportKind.stdio },
		debug: { command: serverPath, transport: TransportKind.stdio },
	};

	const clientOptions = {
		documentSelector: [{ scheme: "file", language: "musi" }],
		synchronize: {
			fileEvents: vscode.workspace.createFileSystemWatcher("**/.clientrc"),
		},
	};

	client = new LanguageClient(
		"musiLsp",
		"Musi Language Server",
		serverOptions,
		clientOptions,
	);

	await Promise.race([
		client.start(),
		new Promise<never>((_, reject) =>
			setTimeout(() => reject(new Error("Server start timeout (30s)")), 30000),
		),
	]);
}

export async function activate(_context: vscode.ExtensionContext) {
	updateStatus("Starting...", "loading");

	try {
		const serverPath = await findServerPath();

		if (!serverPath) {
			updateStatus("Build required", "error");

			const action = await vscode.window.showErrorMessage(
				"Musi LSP server not found. Please run 'dune build' to build server.",
				"Open Terminal",
				"Show Build Instructions",
			);

			if (action === "Open Terminal") {
				const terminal = vscode.window.createTerminal("Musi Build");
				terminal.sendText("dune build");
				terminal.show();
			} else if (action === "Show Build Instructions") {
				vscode.env.openExternal(
					vscode.Uri.parse("https://github.com/musi-lang/musi#Installation"),
				);
			}
			return;
		}

		await startClientWithTimeout(serverPath);

		updateStatus("Ready", "ready");

		setTimeout(() => {
			vscode.window
				.showInformationMessage("Musi language features ready", "Dismiss")
				.then((selection) => {
					if (selection) {
						updateStatus("Ready", "ready");
					}
				});
		}, 1000);
	} catch (e) {
		const error = e instanceof Error ? e.message : String(e);
		updateStatus("Failed", "error");

		const action = await vscode.window.showErrorMessage(
			`Failed to start Musi LSP server: ${error}`,
			"Retry",
			"Show Logs",
		);

		if (action === "Show Logs") {
			vscode.commands.executeCommand("workbench.action.toggleDevTools");
		} else if (action === "Retry") {
			statusBarItem?.dispose();
			vscode.commands.executeCommand("extension.activate", "musi.vscode");
		}
	}
}

export function deactivate() {
	statusBarItem?.dispose();
	return client ? client.stop() : undefined;
}
