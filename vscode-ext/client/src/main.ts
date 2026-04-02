import * as vscode from "vscode";
import { findCliPath, showCliNotFoundUI } from "./bootstrap.ts";
import { MsPackageCodeLensProvider } from "./codelens.ts";
import { clearCliCache, registerCommands } from "./commands.ts";
import { onConfigChange } from "./config.ts";
import { MusiConfigurationProvider } from "./launch.ts";
import { clearCompilerPathCache } from "./runner.ts";
import {
	MUSI_SEMANTIC_LEGEND,
	MusiSemanticTokensProvider,
} from "./semantic.ts";
import { StatusBar } from "./status.ts";

const NOTIFICATION_DELAY_MS = 500;

let _statusBar: StatusBar;

function _setupConfigChangeHandler(context: vscode.ExtensionContext) {
	context.subscriptions.push(
		onConfigChange((_event) => {
			clearCliCache();
			clearCompilerPathCache();
			void _ensureCliAvailability();
		}),
	);
}

async function _ensureCliAvailability() {
	const cliPath = await findCliPath();
	if (!cliPath) {
		_statusBar.update("Musi CLI missing", "error");
		await showCliNotFoundUI();
		return;
	}

	_statusBar.update("Ready", "ready");
	setTimeout(
		() =>
			vscode.window.showInformationMessage(
				"Musi CLI integration ready.",
				"Dismiss",
			),
		NOTIFICATION_DELAY_MS,
	);
}

/**
 * Activate Musi extension.
 * Readies status bar, registers commands, and ensures the CLI is available.
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
			{ scheme: "file", pattern: "**/musi.json" },
			new MsPackageCodeLensProvider(),
		),
	);

	context.subscriptions.push(
		vscode.debug.registerDebugConfigurationProvider(
			"musi",
			new MusiConfigurationProvider(),
		),
	);

	context.subscriptions.push(
		vscode.languages.registerDocumentSemanticTokensProvider(
			{ language: "musi", scheme: "file" },
			new MusiSemanticTokensProvider(),
			MUSI_SEMANTIC_LEGEND,
		),
	);

	await _ensureCliAvailability();
}

/**
 * Deactivate Musi extension.
 * Releases subscriptions and status resources.
 */
export async function deactivate() {
	_statusBar?.dispose();
}
