import * as vscode from "vscode";
import { findServerPath, showServerNotFoundUI } from "./bootstrap";
import { getClient, restartClient, stopClient } from "./client";
import type { StatusBar } from "./status";

type CommandHandler = () => Promise<void> | void;

interface Commands {
	restartServer: CommandHandler;
	stopServer: CommandHandler;
	showLogs: CommandHandler;
}

function _createCommands(statusBar: StatusBar): Commands {
	return {
		async restartServer() {
			statusBar.update("Restarting...", "loading");

			try {
				const serverPath = await findServerPath();
				if (!serverPath) {
					statusBar.update("Server not found", "error");
					await showServerNotFoundUI();
					return;
				}

				await restartClient(serverPath);
				statusBar.update("Ready", "ready");
				vscode.window.showInformationMessage("Musi Language Server restarted successfully.");
			} catch (error) {
				const message = error instanceof Error ? error.message : String(error);
				statusBar.update("Restart failed", "error");
				vscode.window.showErrorMessage(`Unable to restart Musi LSP: ${message}`);
			}
		},

		async stopServer() {
			await stopClient();
			statusBar.update("Stopped", "stopped");
		},

		showLogs() {
			const client = getClient();
			if (client) {
				client.outputChannel.show();
			} else {
				vscode.window.showWarningMessage("Musi Language Server is not running.");
			}
		},
	};
}

/**
 * Register all Musi extension commands with VS Code.
 * @param context Extension context for managing subscriptions.
 * @param statusBar Status bar instance for visual feedback.
 */
export function registerCommands(context: vscode.ExtensionContext, statusBar: StatusBar) {
	const commands = _createCommands(statusBar);

	context.subscriptions.push(
		vscode.commands.registerCommand("musi.restartServer", commands.restartServer),
		vscode.commands.registerCommand("musi.stopServer", commands.stopServer),
		vscode.commands.registerCommand("musi.showLogs", commands.showLogs),
	);
}
