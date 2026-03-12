import * as vscode from "vscode";

/**
 * Extension configuration values from VS Code settings.
 */
export interface Config {
	readonly lspPath: string;
	readonly cliPath: string;
	readonly checkOnSave: boolean;
	readonly serverPath: string | null;
	readonly runtimePath: string | null;
	readonly traceServer: "off" | "messages" | "verbose";
	readonly diagnosticsEnabled: boolean;
	readonly inlayHintsEnabled: boolean;
	readonly completionEnabled: boolean;
	readonly formattingEnabled: boolean;
	readonly formattingIndentSize: number;
}

export const CONFIG_DEFAULTS: Config = {
	lspPath: "music_lsp",
	cliPath: "musi",
	checkOnSave: true,
	serverPath: null,
	runtimePath: null,
	traceServer: "off",
	diagnosticsEnabled: true,
	inlayHintsEnabled: true,
	completionEnabled: true,
	formattingEnabled: true,
	formattingIndentSize: 2,
};

/**
 * Retrieve current Musi extension configuration from VS Code settings.
 * Falls back to defaults for any unset values.
 */
export function getConfig(): Config {
	const cfg = vscode.workspace.getConfiguration("musi");

	return {
		lspPath: cfg.get("lspPath", CONFIG_DEFAULTS.lspPath),
		cliPath: cfg.get("cliPath", CONFIG_DEFAULTS.cliPath),
		checkOnSave: cfg.get("checkOnSave", CONFIG_DEFAULTS.checkOnSave),
		serverPath: cfg.get("server.path", CONFIG_DEFAULTS.serverPath),
		runtimePath: cfg.get("runtime.path", CONFIG_DEFAULTS.runtimePath),
		traceServer: cfg.get("trace.server", CONFIG_DEFAULTS.traceServer),
		diagnosticsEnabled: cfg.get(
			"diagnostics.enable",
			CONFIG_DEFAULTS.diagnosticsEnabled,
		),
		inlayHintsEnabled: cfg.get(
			"inlayHints.enable",
			CONFIG_DEFAULTS.inlayHintsEnabled,
		),
		completionEnabled: cfg.get(
			"completion.enable",
			CONFIG_DEFAULTS.completionEnabled,
		),
		formattingEnabled: cfg.get(
			"formatting.enable",
			CONFIG_DEFAULTS.formattingEnabled,
		),
		formattingIndentSize: cfg.get(
			"formatting.indentSize",
			CONFIG_DEFAULTS.formattingIndentSize,
		),
	};
}

/**
 * Subscribe to configuration changes for Musi settings.
 * @param callback Function to invoke when any `musi.*` setting changes.
 * @returns Disposable to unsubscribe from changes.
 */
export function onConfigChange(
	callback: (event: vscode.ConfigurationChangeEvent) => void,
): vscode.Disposable {
	return vscode.workspace.onDidChangeConfiguration((event) => {
		if (event.affectsConfiguration("musi")) {
			callback(event);
		}
	});
}
