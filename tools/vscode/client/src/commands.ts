import * as vscode from "vscode";
import { findServerPath, showServerNotFoundUI } from "./bootstrap";
import { restartClient } from "./client";
import type { StatusBar } from "./status";

export function createCommands(statusBar: StatusBar) {
	return {
		restartServer: async () => {
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

				vscode.window.showInformationMessage(
					"Musi Language Server restarted successfully",
				);
			} catch (error) {
				const message = error instanceof Error ? error.message : String(error);
				statusBar.update("Failed", "error");
				vscode.window.showErrorMessage(
					`Failed to restart Musi LSP server: ${message}`,
				);
			}
		},

		showLogs: () => {
			vscode.commands.executeCommand("workbench.action.toggleDevTools");
		},

		stopServer: async () => {
			const { stopClient } = await import("./client");
			await stopClient();
			statusBar.update("Stopped", "stopped");
		},
	};
}

export function registerCommands(
	context: vscode.ExtensionContext,
	statusBar: StatusBar,
) {
	const commands = createCommands(statusBar);

	context.subscriptions.push(
		vscode.commands.registerCommand("musi.restartServer", commands.restartServer),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("musi.showLogs", commands.showLogs),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("musi.stopServer", commands.stopServer),
	);
}
