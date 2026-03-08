import * as vscode from "vscode";
import { findServerPath, showServerNotFoundUI } from "./bootstrap";
import { getClient, restartClient, stopClient } from "./client";
import type { StatusBar } from "./status";

type CommandHandler = (...args: unknown[]) => Promise<void> | void;

interface MsPackageTask {
	command: string;
	description?: string;
}

interface MsPackage {
	tasks?: Record<string, string | MsPackageTask>;
}

interface Commands {
	restartServer: CommandHandler;
	stopServer: CommandHandler;
	showLogs: CommandHandler;
	runFile: CommandHandler;
	checkFile: CommandHandler;
	runTask: CommandHandler;
	runTest: CommandHandler;
	runMain: CommandHandler;
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
				vscode.window.showInformationMessage(
					"Musi Language Server restarted successfully.",
				);
			} catch (error) {
				const message = error instanceof Error ? error.message : String(error);
				statusBar.update("Restart failed", "error");
				vscode.window.showErrorMessage(
					`Failed to restart Musi LSP: ${message}`,
				);
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
				vscode.window.showWarningMessage(
					"Musi Language Server is not running.",
				);
			}
		},

		runFile() {
			const editor = vscode.window.activeTextEditor;
			if (!editor) {
				vscode.window.showWarningMessage("No active editor.");
				return;
			}
			const file = editor.document.uri.fsPath;
			const terminal =
				vscode.window.terminals.find((t) => t.name === "Musi") ??
				vscode.window.createTerminal("Musi");
			terminal.show();
			terminal.sendText(`musi run ${JSON.stringify(file)}`);
		},

		checkFile() {
			const editor = vscode.window.activeTextEditor;
			if (!editor) {
				vscode.window.showWarningMessage("No active editor.");
				return;
			}
			const file = editor.document.uri.fsPath;
			const terminal =
				vscode.window.terminals.find((t) => t.name === "Musi") ??
				vscode.window.createTerminal("Musi");
			terminal.show();
			terminal.sendText(`musi check ${JSON.stringify(file)}`);
		},

		async runTask() {
			const pkgFiles = await vscode.workspace.findFiles(
				"mspackage.json",
				"**/node_modules/**",
				1,
			);
			if (!pkgFiles.length) {
				vscode.window.showWarningMessage(
					"No mspackage.json found in workspace.",
				);
				return;
			}
			let pkg: MsPackage;
			try {
				const raw = await vscode.workspace.fs.readFile(pkgFiles[0]);
				pkg = JSON.parse(Buffer.from(raw).toString("utf8")) as MsPackage;
			} catch {
				vscode.window.showErrorMessage("Failed to parse mspackage.json.");
				return;
			}
			if (!pkg.tasks || !Object.keys(pkg.tasks).length) {
				vscode.window.showWarningMessage(
					"No tasks defined in mspackage.json.",
				);
				return;
			}
			const items = Object.entries(pkg.tasks).map(([name, entry]) => ({
				label: name,
				description:
					typeof entry === "string"
						? entry
						: (entry.description ?? entry.command),
			}));
			const pick = await vscode.window.showQuickPick(items, {
				placeHolder: "Select task to run",
			});
			if (pick) {
				const terminal =
					vscode.window.terminals.find((t) => t.name === "Musi") ??
					vscode.window.createTerminal("Musi");
				terminal.show();
				terminal.sendText(`musi task ${pick.label}`);
			}
		},

		runTest(...args: unknown[]) {
			// Called from CodeLens: args[0] = uri string (ignored), args[1] = label (ignored)
			// Just run `musi test <file>` on the active editor.
			const editor = vscode.window.activeTextEditor;
			if (!editor) {
				vscode.window.showWarningMessage("No active editor.");
				return;
			}
			const file = editor.document.uri.fsPath;
			const terminal =
				vscode.window.terminals.find((t) => t.name === "Musi") ??
				vscode.window.createTerminal("Musi");
			terminal.show();
			terminal.sendText(`musi test ${JSON.stringify(file)}`);
		},

		runMain(...args: unknown[]) {
			// Called from CodeLens: args[0] = uri string (ignored)
			// Same as runFile.
			const editor = vscode.window.activeTextEditor;
			if (!editor) {
				vscode.window.showWarningMessage("No active editor.");
				return;
			}
			const file = editor.document.uri.fsPath;
			const terminal =
				vscode.window.terminals.find((t) => t.name === "Musi") ??
				vscode.window.createTerminal("Musi");
			terminal.show();
			terminal.sendText(`musi run ${JSON.stringify(file)}`);
		},
	};
}

/**
 * Register all Musi extension commands with VS Code.
 * @param context Extension context for managing subscriptions.
 * @param statusBar Status bar instance for visual feedback.
 */
export function registerCommands(
	context: vscode.ExtensionContext,
	statusBar: StatusBar,
) {
	const commands = _createCommands(statusBar);

	context.subscriptions.push(
		vscode.commands.registerCommand(
			"musi.restartServer",
			commands.restartServer,
		),
		vscode.commands.registerCommand("musi.stopServer", commands.stopServer),
		vscode.commands.registerCommand("musi.showLogs", commands.showLogs),
		vscode.commands.registerCommand("musi.runFile", commands.runFile),
		vscode.commands.registerCommand("musi.checkFile", commands.checkFile),
		vscode.commands.registerCommand("musi.runTask", commands.runTask),
		vscode.commands.registerCommand("musi.runTest", commands.runTest),
		vscode.commands.registerCommand("musi.runMain", commands.runMain),
	);
}
