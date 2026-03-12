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
import type { MsPackage, MsPackageTask } from "./types";

let _cachedCliPath: string | undefined;

const TERMINAL_NAME = "Musi";

type CommandHandler = (...args: unknown[]) => Promise<void> | void;

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

export function clearCliCache() {
	_cachedCliPath = undefined;
}

function _getOrCreateTerminal(): vscode.Terminal {
	return (
		vscode.window.terminals.find((t) => t.name === TERMINAL_NAME) ??
		vscode.window.createTerminal(TERMINAL_NAME)
	);
}

function _runInDir(cmd: string, dir: string) {
	const terminal = _getOrCreateTerminal();
	terminal.show();
	terminal.sendText(`cd ${JSON.stringify(dir)} && ${cmd}`);
}

async function _runCliOnFile(
	subcommand: string,
	args: unknown[],
): Promise<void> {
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
	const terminal = _getOrCreateTerminal();
	terminal.show();
	terminal.sendText(
		`${JSON.stringify(cliPath)} ${subcommand} ${JSON.stringify(file)}`,
	);
}

function _getTaskCommand(entry: string | MsPackageTask): string {
	return typeof entry === "string" ? entry : entry.command;
}

async function _loadPackageTasks(): Promise<
	{ tasks: Record<string, string | MsPackageTask>; pkgDir: string } | undefined
> {
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
		return undefined;
	}
	let pkg: MsPackage;
	try {
		const raw = await vscode.workspace.fs.readFile(pkgFile);
		pkg = JSON.parse(Buffer.from(raw).toString("utf8")) as MsPackage;
	} catch {
		vscode.window.showErrorMessage("Failed to parse mspackage.json.");
		return undefined;
	}
	if (!pkg.tasks || !Object.keys(pkg.tasks).length) {
		vscode.window.showWarningMessage(
			"No tasks defined in mspackage.json.",
		);
		return undefined;
	}
	return { tasks: pkg.tasks, pkgDir: path.dirname(pkgFile.fsPath) };
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
				const message =
					error instanceof Error ? error.message : String(error);
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
			await _runCliOnFile("run", args);
		},

		async checkFile(...args: unknown[]) {
			await _runCliOnFile("check", args);
		},

		async runTask(...args: unknown[]) {
			const loaded = await _loadPackageTasks();
			if (!loaded) return;
			const { tasks, pkgDir } = loaded;

			const quickName =
				typeof args[0] === "string" ? args[0] : undefined;
			if (quickName && quickName in tasks) {
				const entry = tasks[quickName];
				if (entry) _runInDir(_getTaskCommand(entry), pkgDir);
				return;
			}

			const items = Object.entries(tasks).map(([name, entry]) => ({
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
				const entry = tasks[pick.label];
				if (entry) _runInDir(_getTaskCommand(entry), pkgDir);
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

			_runInDir(cmd, dir);
		},

		async runTest(...args: unknown[]) {
			await _runCliOnFile("test", args);
		},

		async runMain(...args: unknown[]) {
			await _runCliOnFile("run", args);
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
