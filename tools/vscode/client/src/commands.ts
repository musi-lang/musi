import * as path from "node:path";
import * as vscode from "vscode";
import {
	findCliPath,
	findServerPath,
	showCliNotFoundUI,
	showServerNotFoundUI,
} from "./bootstrap";
import { getClient, restartClient, stopClient } from "./client";
import type { StatusBar } from "./status";

let _cachedCliPath: string | undefined;

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
	runTaskCommand: CommandHandler;
	runTest: CommandHandler;
	runMain: CommandHandler;
}

function _createCommands(statusBar: StatusBar): Commands {
	return {
		async restartServer(..._args: unknown[]) {
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

		async stopServer(..._args: unknown[]) {
			await stopClient();
			statusBar.update("Stopped", "stopped");
		},

		showLogs(..._args: unknown[]) {
			const client = getClient();
			if (client) {
				client.outputChannel.show();
			} else {
				vscode.window.showWarningMessage(
					"Musi Language Server is not running.",
				);
			}
		},

		async runFile(...args: unknown[]) {
			// Allow overridable file for testing/automation
			let file: string | undefined;
			if (typeof args[0] === "string") {
				file = args[0];
			}
			if (!file) {
				const editor = vscode.window.activeTextEditor;
				if (!editor) {
					vscode.window.showWarningMessage("No active editor.");
					return;
				}
				file = editor.document.uri.fsPath;
			}
			const cliPath = _cachedCliPath ?? (await findCliPath());
			if (!cliPath) {
				await showCliNotFoundUI();
				return;
			}
			_cachedCliPath = cliPath;
			const terminal =
				vscode.window.terminals.find((t) => t.name === "Musi") ??
				vscode.window.createTerminal("Musi");
			terminal.show();
			terminal.sendText(
				`${JSON.stringify(cliPath)} run ${JSON.stringify(file)}`,
			);
		},

		async checkFile(...args: unknown[]) {
			// Allow overridable file for testing/automation
			let file: string | undefined;
			if (typeof args[0] === "string") {
				file = args[0];
			}
			if (!file) {
				const editor = vscode.window.activeTextEditor;
				if (!editor) {
					vscode.window.showWarningMessage("No active editor.");
					return;
				}
				file = editor.document.uri.fsPath;
			}
			const cliPath = _cachedCliPath ?? (await findCliPath());
			if (!cliPath) {
				await showCliNotFoundUI();
				return;
			}
			_cachedCliPath = cliPath;
			const terminal =
				vscode.window.terminals.find((t) => t.name === "Musi") ??
				vscode.window.createTerminal("Musi");
			terminal.show();
			terminal.sendText(
				`${JSON.stringify(cliPath)} check ${JSON.stringify(file)}`,
			);
		},

		async runTask(...args: unknown[]) {
			const pkgFiles = await vscode.workspace.findFiles(
				"mspackage.json",
				"**/node_modules/**",
				1,
			);
			const pkgFile = pkgFiles[0];
			if (!pkgFile) {
				vscode.window.showWarningMessage(
					"No mspackage.json found in workspace.",
				);
				return;
			}
			let pkg: MsPackage;
			try {
				const raw = await vscode.workspace.fs.readFile(pkgFile);
				pkg = JSON.parse(Buffer.from(raw).toString("utf8")) as MsPackage;
			} catch {
				vscode.window.showErrorMessage("Failed to parse mspackage.json.");
				return;
			}
			if (!pkg.tasks || !Object.keys(pkg.tasks).length) {
				vscode.window.showWarningMessage("No tasks defined in mspackage.json.");
				return;
			}

			// Try to use first arg (task name) if provided (e.g. for automation)
			const quickName = typeof args[0] === "string" ? args[0] : undefined;
			if (quickName && quickName in pkg.tasks) {
				const taskEntry = pkg.tasks[quickName];
				if (!taskEntry) {
					vscode.window.showErrorMessage(
						`Task "${quickName}" missing in mspackage.json.`,
					);
					return;
				}
				const cmd =
					typeof taskEntry === "string" ? taskEntry : taskEntry.command;
				const pkgDir = path.dirname(pkgFile.fsPath);
				const terminal =
					vscode.window.terminals.find((t) => t.name === "Musi") ??
					vscode.window.createTerminal("Musi");
				terminal.show();
				terminal.sendText(`cd ${JSON.stringify(pkgDir)} && ${cmd}`);
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
				const taskEntry = pkg.tasks[pick.label];
				if (!taskEntry) {
					vscode.window.showErrorMessage(
						`Task "${pick.label}" missing in mspackage.json.`,
					);
					return;
				}
				const cmd =
					typeof taskEntry === "string" ? taskEntry : taskEntry.command;
				const pkgDir = path.dirname(pkgFile.fsPath);
				const terminal =
					vscode.window.terminals.find((t) => t.name === "Musi") ??
					vscode.window.createTerminal("Musi");
				terminal.show();
				terminal.sendText(`cd ${JSON.stringify(pkgDir)} && ${cmd}`);
			}
		},

		async runTaskCommand(...args: unknown[]) {
			const cmd = typeof args[0] === "string" ? args[0] : "";
			const dir = typeof args[1] === "string" ? args[1] : ".";

			// Warn if task uses `musi` but CLI not found
			if (cmd.includes("musi") && !_cachedCliPath) {
				const cliPath = await findCliPath();
				if (!cliPath) {
					vscode.window.showWarningMessage(
						"Musi CLI not found in PATH. Task may fail if it uses 'musi' command.",
					);
				} else {
					_cachedCliPath = cliPath;
				}
			}

			const terminal =
				vscode.window.terminals.find((t) => t.name === "Musi") ??
				vscode.window.createTerminal("Musi");
			terminal.show();
			terminal.sendText(`cd ${JSON.stringify(dir)} && ${cmd}`);
		},

		async runTest(...args: unknown[]) {
			// Try to use first arg (file)
			let file: string | undefined;
			if (typeof args[0] === "string") {
				file = args[0];
			}
			if (!file) {
				const editor = vscode.window.activeTextEditor;
				if (!editor) {
					vscode.window.showWarningMessage("No active editor.");
					return;
				}
				file = editor.document.uri.fsPath;
			}
			const cliPath = _cachedCliPath ?? (await findCliPath());
			if (!cliPath) {
				await showCliNotFoundUI();
				return;
			}
			_cachedCliPath = cliPath;
			const terminal =
				vscode.window.terminals.find((t) => t.name === "Musi") ??
				vscode.window.createTerminal("Musi");
			terminal.show();
			terminal.sendText(
				`${JSON.stringify(cliPath)} test ${JSON.stringify(file)}`,
			);
		},

		async runMain(...args: unknown[]) {
			// Try to use first arg (file)
			let file: string | undefined;
			if (typeof args[0] === "string") {
				file = args[0];
			}
			if (!file) {
				const editor = vscode.window.activeTextEditor;
				if (!editor) {
					vscode.window.showWarningMessage("No active editor.");
					return;
				}
				file = editor.document.uri.fsPath;
			}
			const cliPath = _cachedCliPath ?? (await findCliPath());
			if (!cliPath) {
				await showCliNotFoundUI();
				return;
			}
			_cachedCliPath = cliPath;
			const terminal =
				vscode.window.terminals.find((t) => t.name === "Musi") ??
				vscode.window.createTerminal("Musi");
			terminal.show();
			terminal.sendText(
				`${JSON.stringify(cliPath)} run ${JSON.stringify(file)}`,
			);
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
		vscode.commands.registerCommand(
			"musi.runTaskCommand",
			commands.runTaskCommand,
		),
		vscode.commands.registerCommand("musi.runTest", commands.runTest),
		vscode.commands.registerCommand("musi.runMain", commands.runMain),
	);
}
