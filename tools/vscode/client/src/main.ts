import * as vscode from "vscode";
import { findServerPath, showServerNotFoundUI } from "./bootstrap";
import { createAndStartClient, getClient, stopClient } from "./client";
import { registerCommands } from "./commands";
import { onConfigChange } from "./config";
import { StatusBar } from "./status";

let _statusBar: StatusBar;

export async function activate(
	context: vscode.ExtensionContext,
) {
	_statusBar = new StatusBar();
	context.subscriptions.push({ dispose: () => _statusBar.dispose() });

	_statusBar.update("Starting...", "loading");

	registerCommands(context, _statusBar);

	context.subscriptions.push(
		onConfigChange(async () => {
			const client = getClient();
			if (client) {
				await client.sendNotification("workspace/didChangeConfiguration", {
					settings: {},
				});
			}
		}),
	);

	try {
		const serverPath = await findServerPath();
		if (!serverPath) {
			_statusBar.update("Server not found", "error");
			await showServerNotFoundUI();
			return;
		}

		await createAndStartClient(serverPath);
		_statusBar.update("Ready", "ready");

		setTimeout(() => {
			vscode.window
				.showInformationMessage("Musi language features ready", "Dismiss")
				.then(() => {
                    /* keep being ready */
				});
		}, 500);
	} catch (error) {
		const message = error instanceof Error ? error.message : String(error);
		_statusBar.update("Failed", "error");

		const action = await vscode.window.showErrorMessage(
			`Unable to start Musi LSP server: ${message}`,
			"Retry",
			"Show Logs",
		);

		if (action === "Show Logs") {
			vscode.commands.executeCommand("workbench.action.toggleDevTools");
		} else if (action === "Retry") {
			vscode.commands.executeCommand("musi.restartServer");
		}
	}
}

export async function deactivate() {
	_statusBar?.dispose();
	await stopClient();
}
