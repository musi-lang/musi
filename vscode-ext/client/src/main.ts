import * as vscode from "vscode";
import { findCliPath } from "./bootstrap.ts";
import { MsPackageCodeLensProvider } from "./codelens.ts";
import { clearCliCache, registerCommands } from "./commands.ts";
import { onConfigChange } from "./config.ts";
import { DiagnosticsController } from "./diagnostics.ts";
import { LspController } from "./lsp.ts";
import { StatusBar } from "./status.ts";

let statusBar: StatusBar | undefined;
let diagnostics: DiagnosticsController | undefined;
let lsp: LspController | undefined;

async function refreshCliAndStatus() {
	if (!statusBar || !diagnostics) {
		return;
	}
	if (!lsp?.isRunning() && !findCliPath()) {
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
			void (async () => {
				if (lsp) {
					await lsp.restart(context);
				}
				await refreshCliAndStatus();
			})();
		}),
	);
}

export async function activate(context: vscode.ExtensionContext) {
	statusBar = new StatusBar();
	diagnostics = new DiagnosticsController(statusBar);
	lsp = new LspController(statusBar, diagnostics);

	context.subscriptions.push(
		statusBar,
		diagnostics,
		lsp,
		vscode.languages.registerCodeLensProvider(
			{ scheme: "file", pattern: "**/musi.json" },
			new MsPackageCodeLensProvider(),
		),
	);

	registerCommands(context, diagnostics);
	registerEditorListeners(context);
	setupConfigChangeHandler(context);

	await lsp.start(context);
	await refreshCliAndStatus();
}

export async function deactivate() {
	await lsp?.stop();
	diagnostics?.dispose();
	statusBar?.dispose();
}
