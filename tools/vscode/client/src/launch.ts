import * as vscode from "vscode";
import { buildExecutionRequest, executeInTerminal } from "./runner";

export class MusiConfigurationProvider
	implements vscode.DebugConfigurationProvider
{
	resolveDebugConfiguration(
		_folder: vscode.WorkspaceFolder | undefined,
		config: vscode.DebugConfiguration,
		_token?: vscode.CancellationToken,
	): vscode.ProviderResult<vscode.DebugConfiguration> {
		if (!config["type"] && !config["request"] && !config["name"]) {
			const editor = vscode.window.activeTextEditor;
			if (editor && editor.document.languageId === "musi") {
				config["type"] = "musi";
				config["request"] = "launch";
				config["name"] = "Run File";
				config["file"] = editor.document.uri.fsPath;
			}
		}

		if (!config["file"]) {
			const editor = vscode.window.activeTextEditor;
			if (editor) {
				config["file"] = editor.document.uri.fsPath;
			}
		}

		return config;
	}

	async resolveDebugConfigurationWithSubstitutedVariables(
		_folder: vscode.WorkspaceFolder | undefined,
		config: vscode.DebugConfiguration,
		_token?: vscode.CancellationToken,
	): Promise<vscode.DebugConfiguration | undefined> {
		const request = buildExecutionRequest(config["file"] ?? "", {
			name: config["name"] ?? "Launch",
			file: config["file"],
			compilerArgs: config["compilerArgs"],
			runtimeArgs: config["runtimeArgs"],
			env: config["env"],
			envFile: config["envFile"],
			cwd: config["cwd"],
			preLaunchTask: config["preLaunchTask"],
		});

		await executeInTerminal(request, "run");

		// Return undefined to prevent VS Code from trying to start a debug session
		// (no DAP server yet)
		return undefined;
	}
}
