import * as vscode from "vscode";

export interface Config {
	readonly serverPath: string | null;
	readonly runtimePath: string | null;
	readonly traceServer: "off" | "messages" | "verbose";
	readonly diagnosticsEnabled: boolean;
	readonly inlayHintsEnabled: boolean;
	readonly completionEnabled: boolean;
	readonly formattingEnabled: boolean;
	readonly formattingIndentSize: number;
}

export function getConfig(): Config {
	const config = vscode.workspace.getConfiguration("musi");
	return {
		serverPath: config.get<string | null>("server.path", null),
		runtimePath: config.get<string | null>("runtime.path", null),
		traceServer: config.get<"off" | "messages" | "verbose">(
			"trace.server",
			"off",
		),
		diagnosticsEnabled: config.get<boolean>("diagnostics.enable", true),
		inlayHintsEnabled: config.get<boolean>("inlayHints.enable", true),
		completionEnabled: config.get<boolean>("completion.enable", true),
		formattingEnabled: config.get<boolean>("formatting.enable", true),
		formattingIndentSize: config.get<number>("formatting.indentSize", 2),
	};
}

export function onConfigChange(callback: () => void): vscode.Disposable {
	return vscode.workspace.onDidChangeConfiguration((event) => {
		if (event.affectsConfiguration("musi")) {
			callback();
		}
	});
}
