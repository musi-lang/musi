import * as fs from "node:fs";
import * as path from "node:path";
import { window, workspace } from "vscode";
import { LanguageClient, TransportKind } from "vscode-languageclient/node.js";

/**
 * @type {{ start: () => void; stop: () => any; }}
 */
let client;

/**
 * @param {{ subscriptions: import("vscode").Disposable[]; extensionPath: string }} _context
 */
export function activate(_context) {

	const serverSource = path.join(
		_context.extensionPath,
		"../../_build/default/tools/lsp/bin/main.exe",
	);


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

	client.start();
	window.showInformationMessage("Musi LSP server started!");
}

export function deactivate() {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
