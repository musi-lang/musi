import * as vscode from "vscode";
import { findServerPath, showServerNotFoundUI } from "./bootstrap.ts";
import { createAndStartClient, getClient, stopClient } from "./client.ts";
import {
	MsPackageCodeLensProvider,
	MsTestCodeLensProvider,
} from "./codelens.ts";
import { clearCliCache, registerCommands } from "./commands.ts";
import { onConfigChange } from "./config.ts";
import { MusiConfigurationProvider } from "./launch.ts";
import { registerMspackageValidator } from "./mspackage-validator.ts";
import { clearCompilerPathCache } from "./runner.ts";
import { StatusBar } from "./status.ts";

const NOTIFICATION_DELAY_MS = 500;

let _statusBar: StatusBar;

function _setupConfigChangeHandler(context: vscode.ExtensionContext) {
	context.subscriptions.push(
		onConfigChange(async (event) => {
			if (event.affectsConfiguration("musi.cliPath")) {
				clearCliCache();
			}
			if (event.affectsConfiguration("musi.compiler.path")) {
				clearCompilerPathCache();
			}
			const client = getClient();
			if (client) {
				await client.sendNotification("workspace/didChangeConfiguration", {
					settings: {},
				});
			}
		}),
	);
}

async function _startServer() {
	const serverPath = await findServerPath();

	if (!serverPath) {
		_statusBar.update("Server not found", "error");
		await showServerNotFoundUI();
		return;
	}

	await createAndStartClient(serverPath);
	_statusBar.update("Ready", "ready");

	// Delay avoids notification flicker on fast server starts
	setTimeout(() => {
		vscode.window.showInformationMessage(
			"Musi language features ready.",
			"Dismiss",
		);
	}, NOTIFICATION_DELAY_MS);
}

async function _handleActivationError(error: unknown) {
	const message = error instanceof Error ? error.message : String(error);
	_statusBar.update("Failed to start", "error");

	const action = await vscode.window.showErrorMessage(
		`Failed to start Musi LSP server: ${message}`,
		"Retry",
		"Show Logs",
	);

	if (action === "Show Logs") {
		vscode.commands.executeCommand("workbench.action.toggleDevTools");
	} else if (action === "Retry") {
		vscode.commands.executeCommand("musi.restartServer");
	}
}

/**
 * Activate Musi extension.
 * Readies language client, status bar, and registers commands.
 * @param context VS Code extension context.
 */
export async function activate(context: vscode.ExtensionContext) {
	_statusBar = new StatusBar();
	context.subscriptions.push({ dispose: () => _statusBar.dispose() });

	_statusBar.update("Starting...", "loading");

	registerCommands(context, _statusBar);
	_setupConfigChangeHandler(context);

	context.subscriptions.push(
		vscode.languages.registerCodeLensProvider(
			{ scheme: "file", pattern: "**/mspackage.toml" },
			new MsPackageCodeLensProvider(),
		),
		vscode.languages.registerCodeLensProvider(
			{ language: "musi", pattern: "**/*.test.ms" },
			new MsTestCodeLensProvider(),
		),
	);

	context.subscriptions.push(
		vscode.debug.registerDebugConfigurationProvider(
			"musi",
			new MusiConfigurationProvider(),
		),
	);

	registerMspackageValidator(context);

	try {
		await _startServer();
	} catch (error) {
		await _handleActivationError(error);
	}
}

/**
 * Deactivate Musi extension.
 * Stops language client and cleans up resources.
 */
export async function deactivate() {
	_statusBar?.dispose();
	await stopClient();
}
