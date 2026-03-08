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

const _DEFAULTS: Config = {
	lspPath: "musi-lsp",
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

function _get<T>(
	cfg: vscode.WorkspaceConfiguration,
	key: string,
	fallback: T,
): T {
	return cfg.get<T>(key, fallback);
}

/**
 * Retrieve current Musi extension configuration from VS Code settings.
 * Falls back to defaults for any unset values.
 */
export function getConfig(): Config {
	const cfg = vscode.workspace.getConfiguration("musi");

	return {
		lspPath: _get(cfg, "lspPath", _DEFAULTS.lspPath),
		cliPath: _get(cfg, "cliPath", _DEFAULTS.cliPath),
		checkOnSave: _get(cfg, "checkOnSave", _DEFAULTS.checkOnSave),
		serverPath: _get(cfg, "server.path", _DEFAULTS.serverPath),
		runtimePath: _get(cfg, "runtime.path", _DEFAULTS.runtimePath),
		traceServer: _get(cfg, "trace.server", _DEFAULTS.traceServer),
		diagnosticsEnabled: _get(
			cfg,
			"diagnostics.enable",
			_DEFAULTS.diagnosticsEnabled,
		),
		inlayHintsEnabled: _get(
			cfg,
			"inlayHints.enable",
			_DEFAULTS.inlayHintsEnabled,
		),
		completionEnabled: _get(
			cfg,
			"completion.enable",
			_DEFAULTS.completionEnabled,
		),
		formattingEnabled: _get(
			cfg,
			"formatting.enable",
			_DEFAULTS.formattingEnabled,
		),
		formattingIndentSize: _get(
			cfg,
			"formatting.indentSize",
			_DEFAULTS.formattingIndentSize,
		),
	};
}

/**
 * Subscribe to configuration changes for Musi settings.
 * @param callback Function to invoke when any `musi.*` setting changes.
 * @returns Disposable to unsubscribe from changes.
 */
export function onConfigChange(callback: () => void): vscode.Disposable {
	return vscode.workspace.onDidChangeConfiguration((event) => {
		if (event.affectsConfiguration("musi")) {
			callback();
		}
	});
}
