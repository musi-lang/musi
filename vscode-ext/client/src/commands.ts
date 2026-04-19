import * as vscode from "vscode";
import { getConfig } from "./config.ts";
import type { DiagnosticsController } from "./diagnostics.ts";
import { formatActiveDocumentWithCli } from "./formatter/formatter.ts";
import type { LspController } from "./lsp.ts";
import {
	activeDocumentUri,
	findOwningManifestPathForUri,
	findWorkspaceManifestPathForUri,
	loadPackageRoot,
	taskPlan,
	taskSpecs,
} from "./manifest/manifest.ts";
import {
	buildPackageExecutionRequest,
	executePackageCommandInTerminal,
	executeTaskPlanInTerminal,
} from "./runner.ts";

type CommandHandler = (...args: unknown[]) => Promise<void> | void;

const RUNTIME_ARGS_SPLIT_REGEX = /\s+/;

interface Commands {
	runPackageEntry: CommandHandler;
	checkPackage: CommandHandler;
	buildPackage: CommandHandler;
	runPackageTests: CommandHandler;
	runTask: CommandHandler;
	runTaskByName: CommandHandler;
	selectRunConfiguration: CommandHandler;
	runWithArgs: CommandHandler;
	editRunConfigurations: CommandHandler;
	fmt: CommandHandler;
	showActions: CommandHandler;
	restartLsp: CommandHandler;
	startLsp: CommandHandler;
	stopLsp: CommandHandler;
	showLspOutput: CommandHandler;
	checkWorkspace: CommandHandler;
	buildWorkspace: CommandHandler;
	runWorkspaceTests: CommandHandler;
	fmtWorkspace: CommandHandler;
}

function activeUriFromArgs(args: readonly unknown[]): vscode.Uri | undefined {
	const candidate = args[0];
	if (candidate instanceof vscode.Uri) {
		return candidate;
	}
	if (typeof candidate === "string") {
		return candidate.startsWith("file://")
			? vscode.Uri.parse(candidate)
			: vscode.Uri.file(candidate);
	}
	return activeDocumentUri();
}

async function packageRootFromArgs(args: readonly unknown[]) {
	const manifestPath = findOwningManifestPathForUri(activeUriFromArgs(args));
	if (!manifestPath) {
		vscode.window.showWarningMessage(
			"No owning musi.json for the selected file.",
		);
		return undefined;
	}
	try {
		return await loadPackageRoot(manifestPath);
	} catch (error) {
		vscode.window.showErrorMessage(
			`Failed to read owning musi.json: ${String(error)}`,
		);
		return undefined;
	}
}

async function workspacePackageRootFromArgs(args: readonly unknown[]) {
	const manifestPath = findWorkspaceManifestPathForUri(activeUriFromArgs(args));
	if (!manifestPath) {
		vscode.window.showWarningMessage(
			"No owning musi.json for the selected file.",
		);
		return undefined;
	}
	try {
		return await loadPackageRoot(manifestPath);
	} catch (error) {
		vscode.window.showErrorMessage(
			`Failed to read owning musi.json: ${String(error)}`,
		);
		return undefined;
	}
}

function workspaceRequest(pkg: Awaited<ReturnType<typeof workspacePackageRootFromArgs>>) {
	if (!pkg) {
		return undefined;
	}
	return buildPackageExecutionRequest(pkg, {
		name: "Workspace",
		cliArgs: ["--workspace"],
	});
}

function createCommands(
	context: vscode.ExtensionContext,
	diagnostics: DiagnosticsController,
	lsp: LspController,
): Commands {
	return {
		async runPackageEntry(...args: unknown[]) {
			const pkg = await packageRootFromArgs(args);
			if (!pkg) {
				return;
			}
			const request = buildPackageExecutionRequest(pkg);
			await executePackageCommandInTerminal(request, "run");
		},

		async checkPackage(...args: unknown[]) {
			const pkg = await packageRootFromArgs(args);
			if (!pkg) {
				return;
			}
			await diagnostics.checkManifestPath(pkg.manifestPath);
		},

		async buildPackage(...args: unknown[]) {
			const pkg = await packageRootFromArgs(args);
			if (!pkg) {
				return;
			}
			const request = buildPackageExecutionRequest(pkg);
			await executePackageCommandInTerminal(request, "build");
		},

		async runPackageTests(...args: unknown[]) {
			const pkg = await packageRootFromArgs(args);
			if (!pkg) {
				return;
			}
			const request = buildPackageExecutionRequest(pkg);
			await executePackageCommandInTerminal(request, "test");
		},

		async runTask(...args: unknown[]) {
			const pkg = await packageRootFromArgs(args);
			if (!pkg) {
				return;
			}

			const tasks = taskSpecs(pkg);
			if (tasks.length === 0) {
				vscode.window.showWarningMessage(
					"No tasks defined in the owning musi.json.",
				);
				return;
			}

			const items: vscode.QuickPickItem[] = tasks.map((task) => {
				const item: vscode.QuickPickItem = {
					label: task.name,
					description: task.description ?? task.command,
				};
				if (task.dependencies.length > 0) {
					item.detail = `depends on ${task.dependencies.join(", ")}`;
				}
				return item;
			});

			const pick = await vscode.window.showQuickPick(items, {
				placeHolder: "Select Musi task",
			});
			if (!pick) {
				return;
			}

			try {
				const plan = taskPlan(pkg, pick.label);
				const request = buildPackageExecutionRequest(pkg);
				await executeTaskPlanInTerminal(request, plan);
			} catch (error) {
				vscode.window.showErrorMessage(String(error));
			}
		},

		async runTaskByName(...args: unknown[]) {
			const manifestArg = typeof args[0] === "string" ? args[0] : undefined;
			const taskName = typeof args[1] === "string" ? args[1] : undefined;
			const manifestUri = manifestArg
				? vscode.Uri.parse(manifestArg)
				: activeDocumentUri();
			const manifestPath = findOwningManifestPathForUri(manifestUri);
			if (!(manifestPath && taskName)) {
				vscode.window.showWarningMessage(
					"Task command requires an owning musi.json and task name.",
				);
				return;
			}
			try {
				const pkg = await loadPackageRoot(manifestPath);
				const plan = taskPlan(pkg, taskName);
				const request = buildPackageExecutionRequest(pkg);
				await executeTaskPlanInTerminal(request, plan);
			} catch (error) {
				vscode.window.showErrorMessage(String(error));
			}
		},

		async selectRunConfiguration(...args: unknown[]) {
			const pkg = await packageRootFromArgs(args);
			if (!pkg) {
				return;
			}
			const configs = getConfig().runConfigurations;
			if (configs.length === 0) {
				vscode.window.showWarningMessage("No Musi run configurations defined.");
				return;
			}

			const pick = await vscode.window.showQuickPick(
				configs.map((config) => ({
					label: config.name,
					description: config.entry ?? pkg.mainEntry,
				})),
				{ placeHolder: "Select Musi run configuration" },
			);
			if (!pick) {
				return;
			}

			const selected = configs.find((config) => config.name === pick.label);
			if (!selected) {
				return;
			}

			try {
				const request = buildPackageExecutionRequest(pkg, selected);
				const preLaunchPlan = selected.preLaunchTask
					? taskPlan(pkg, selected.preLaunchTask)
					: [];
				await executePackageCommandInTerminal(request, "run", preLaunchPlan);
			} catch (error) {
				vscode.window.showErrorMessage(String(error));
			}
		},

		async runWithArgs(...args: unknown[]) {
			const pkg = await packageRootFromArgs(args);
			if (!pkg) {
				return;
			}
			const value = await vscode.window.showInputBox({
				prompt: "Runtime arguments for the package entry",
				placeHolder: "--flag value",
			});
			if (value === undefined) {
				return;
			}
			const runtimeArgs = value.split(RUNTIME_ARGS_SPLIT_REGEX).filter(Boolean);
			const request = buildPackageExecutionRequest(pkg, {
				name: "Run with Arguments",
				runtimeArgs,
			});
			await executePackageCommandInTerminal(request, "run");
		},

		async editRunConfigurations() {
			await vscode.commands.executeCommand(
				"workbench.action.openSettings",
				"musi.runConfigurations",
			);
		},

		async fmt() {
			await formatActiveDocumentWithCli();
		},

		async showActions(...args: unknown[]) {
			const items: Array<vscode.QuickPickItem & { command: string }> = [
				{
					label: "Restart LSP",
					description: "Stop and start Musi language server",
					command: "musi.restartLsp",
				},
				{
					label: "Start LSP",
					description: "Start Musi language server",
					command: "musi.startLsp",
				},
				{
					label: "Stop LSP",
					description: "Stop Musi language server",
					command: "musi.stopLsp",
				},
				{
					label: "Show LSP Output",
					description: "Open Musi LSP output channel",
					command: "musi.showLspOutput",
				},
				{
					label: "Check Package",
					description: "Run package diagnostics",
					command: "musi.checkPackage",
				},
				{
					label: "Check Workspace",
					description: "Run `musi check --workspace`",
					command: "musi.checkWorkspace",
				},
				{
					label: "Run Workspace Tests",
					description: "Run `musi test --workspace`",
					command: "musi.runWorkspaceTests",
				},
				{
					label: "Build Workspace",
					description: "Run `musi build --workspace`",
					command: "musi.buildWorkspace",
				},
				{
					label: "Format Workspace",
					description: "Run `musi fmt --all`",
					command: "musi.fmtWorkspace",
				},
			];
			const pick = await vscode.window.showQuickPick(items, {
				placeHolder: "Select Musi action",
			});
			if (pick) {
				await vscode.commands.executeCommand(pick.command, ...args);
			}
		},

		async restartLsp() {
			const ok = await lsp.restart(context);
			const message = ok ? "Musi LSP restarted." : "Musi LSP did not start.";
			await vscode.window.showInformationMessage(message);
		},

		async startLsp() {
			const ok = await lsp.start(context);
			const message = ok ? "Musi LSP started." : "Musi LSP did not start.";
			await vscode.window.showInformationMessage(message);
		},

		async stopLsp() {
			await lsp.stop();
			await vscode.window.showInformationMessage("Musi LSP stopped.");
		},

		showLspOutput() {
			lsp.showOutput();
		},

		async checkWorkspace(...args: unknown[]) {
			const request = workspaceRequest(await workspacePackageRootFromArgs(args));
			if (!request) {
				return;
			}
			await executePackageCommandInTerminal(request, "check");
		},

		async buildWorkspace(...args: unknown[]) {
			const request = workspaceRequest(await workspacePackageRootFromArgs(args));
			if (!request) {
				return;
			}
			await executePackageCommandInTerminal(request, "build");
		},

		async runWorkspaceTests(...args: unknown[]) {
			const request = workspaceRequest(await workspacePackageRootFromArgs(args));
			if (!request) {
				return;
			}
			await executePackageCommandInTerminal(request, "test");
		},

		async fmtWorkspace(...args: unknown[]) {
			const pkg = await workspacePackageRootFromArgs(args);
			if (!pkg) {
				return;
			}
			const request = buildPackageExecutionRequest(pkg, {
				name: "Format Workspace",
				cliArgs: ["--all"],
			});
			await executePackageCommandInTerminal(request, "fmt");
		},
	};
}

export function clearCliCache() {
	// CLI lookup is stateless; the command surface keeps this hook for config refreshes.
}

export function registerCommands(
	context: vscode.ExtensionContext,
	diagnostics: DiagnosticsController,
	lsp: LspController,
) {
	const commands = createCommands(context, diagnostics, lsp);

	context.subscriptions.push(
		vscode.commands.registerCommand(
			"musi.runPackageEntry",
			commands.runPackageEntry,
		),
		vscode.commands.registerCommand("musi.checkPackage", commands.checkPackage),
		vscode.commands.registerCommand("musi.buildPackage", commands.buildPackage),
		vscode.commands.registerCommand(
			"musi.runPackageTests",
			commands.runPackageTests,
		),
		vscode.commands.registerCommand("musi.runTask", commands.runTask),
		vscode.commands.registerCommand(
			"musi.runTaskByName",
			commands.runTaskByName,
		),
		vscode.commands.registerCommand(
			"musi.selectRunConfiguration",
			commands.selectRunConfiguration,
		),
		vscode.commands.registerCommand("musi.runWithArgs", commands.runWithArgs),
		vscode.commands.registerCommand(
			"musi.editRunConfigurations",
			commands.editRunConfigurations,
		),
		vscode.commands.registerCommand("musi.fmt", commands.fmt),
		vscode.commands.registerCommand("musi.showActions", commands.showActions),
		vscode.commands.registerCommand("musi.restartLsp", commands.restartLsp),
		vscode.commands.registerCommand("musi.startLsp", commands.startLsp),
		vscode.commands.registerCommand("musi.stopLsp", commands.stopLsp),
		vscode.commands.registerCommand("musi.showLspOutput", commands.showLspOutput),
		vscode.commands.registerCommand("musi.checkWorkspace", commands.checkWorkspace),
		vscode.commands.registerCommand("musi.buildWorkspace", commands.buildWorkspace),
		vscode.commands.registerCommand(
			"musi.runWorkspaceTests",
			commands.runWorkspaceTests,
		),
		vscode.commands.registerCommand("musi.fmtWorkspace", commands.fmtWorkspace),
	);
}
