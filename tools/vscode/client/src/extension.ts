import * as fs from "node:fs";
import * as vscode from "vscode";
import { window, workspace } from "vscode";
import { LanguageClient, TransportKind } from "vscode-languageclient/node";

let client: LanguageClient;

export async function activate(_context: vscode.ExtensionContext) {
	vscode.window.showInformationMessage("Musi extension activating...");


	try {
		const serverSource = "/Users/krystian/CodeProjects/musi/_build/default/tools/lsp/bin/main.exe";
		vscode.window.showInformationMessage(`Looking for server at: ${serverSource}`);

		if (!fs.existsSync(serverSource)) {
			window.showErrorMessage(`Musi LSP server not found at ${serverSource}. Please run 'dune build' first.`);
			return;
		}

		const serverOptions = {
			run: { command: serverSource, transport: TransportKind.stdio },
			debug: { command: serverSource, transport: TransportKind.stdio },
		};

		const clientOptions = {
			documentSelector: [{ scheme: "file", language: "musi" }],
			synchronize: {
				fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
			},
		};

		client = new LanguageClient(
			"musiLsp",
			"Musi Language Server",
			serverOptions,
			clientOptions,
		);

		await client.start();
		window.showInformationMessage("Musi LSP server started!");
	} catch (e) {
		window.showErrorMessage(`Musi extension failed to activate: ${e}`);
	}
}

export function deactivate() {
	if (!client) return undefined;
	return client.stop();
}
