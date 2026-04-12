import * as vscode from "vscode";
import { getConfig } from "./config.ts";
import type { DiagnosticsController } from "./diagnostics.ts";
import {
	activeDocumentUri,
	findOwningManifestPathForUri,
	loadPackageRoot,
	taskPlan,
	taskSpecs,
} from "./manifest.ts";
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

function createCommands(diagnostics: DiagnosticsController): Commands {
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
	};
}

export function clearCliCache() {
	// CLI lookup is stateless; the command surface keeps this hook for config refreshes.
}

export function registerCommands(
	context: vscode.ExtensionContext,
	diagnostics: DiagnosticsController,
) {
	const commands = createCommands(diagnostics);

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
	);
}
