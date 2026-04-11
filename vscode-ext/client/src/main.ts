import * as vscode from "vscode";
import { findCliPath } from "./bootstrap.ts";
import { MsPackageCodeLensProvider } from "./codelens.ts";
import { clearCliCache, registerCommands } from "./commands.ts";
import { onConfigChange } from "./config.ts";
import { DiagnosticsController } from "./diagnostics.ts";
import { StatusBar } from "./status.ts";

let statusBar: StatusBar | undefined;
let diagnostics: DiagnosticsController | undefined;

async function refreshCliAndStatus() {
	if (!statusBar || !diagnostics) {
		return;
	}
	if (!findCliPath()) {
		statusBar.update("CLI missing", "error");
		return;
	}
	await diagnostics.refreshStatusForActiveEditor();
}

function registerEditorListeners(context: vscode.ExtensionContext) {
	if (!diagnostics) {
		return;
	}

	context.subscriptions.push(
		vscode.workspace.onDidSaveTextDocument((document) => {
			diagnostics?.scheduleDocumentCheck(document);
		}),
		vscode.window.onDidChangeActiveTextEditor(() => {
			void refreshCliAndStatus();
		}),
	);
}

function setupConfigChangeHandler(context: vscode.ExtensionContext) {
	context.subscriptions.push(
		onConfigChange(() => {
			clearCliCache();
			void refreshCliAndStatus();
		}),
	);
}

export async function activate(context: vscode.ExtensionContext) {
	statusBar = new StatusBar();
	diagnostics = new DiagnosticsController(statusBar);

	context.subscriptions.push(
		statusBar,
		diagnostics,
		vscode.languages.registerCodeLensProvider(
			{ scheme: "file", pattern: "**/musi.json" },
			new MsPackageCodeLensProvider(),
		),
	);

	registerCommands(context, diagnostics);
	registerEditorListeners(context);
	setupConfigChangeHandler(context);

	await refreshCliAndStatus();
}

export async function deactivate() {
	diagnostics?.dispose();
	statusBar?.dispose();
}
