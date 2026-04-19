import * as vscode from "vscode";

export interface RuntimeConfig {
	readonly args: string[];
	readonly env: Record<string, string>;
	readonly envFile: string;
	readonly cwd: string;
}

export interface TerminalConfig {
	readonly clearBeforeRun: boolean;
	readonly focusOnRun: boolean;
	readonly reuseTerminal: boolean;
}

export type ParameterNameInlayHints = "none" | "literals" | "all";

export interface InlayHintsConfig {
	readonly enabled: boolean;
	readonly parameterNames: ParameterNameInlayHints;
	readonly parameterNamesSuppressWhenArgumentMatchesName: boolean;
	readonly variableTypes: boolean;
	readonly variableTypesSuppressWhenTypeMatchesName: boolean;
}

export interface RunConfiguration {
	readonly name: string;
	readonly entry?: string;
	readonly cliArgs?: string[];
	readonly runtimeArgs?: string[];
	readonly env?: Record<string, string>;
	readonly envFile?: string;
	readonly cwd?: string;
	readonly preLaunchTask?: string;
}

export interface Config {
	readonly cliPath: string;
	readonly lspPath: string;
	readonly lspEnabled: boolean;
	readonly checkOnSave: boolean;
	readonly hoverMaximumLength: number;
	readonly inlayHints: InlayHintsConfig;
	readonly runtime: RuntimeConfig;
	readonly terminal: TerminalConfig;
	readonly runConfigurations: RunConfiguration[];
}

const RUNTIME_DEFAULTS: RuntimeConfig = {
	args: [],
	env: {},
	envFile: "",
	cwd: "",
};

const TERMINAL_DEFAULTS: TerminalConfig = {
	clearBeforeRun: false,
	focusOnRun: true,
	reuseTerminal: true,
};

const INLAY_HINTS_DEFAULTS: InlayHintsConfig = {
	enabled: true,
	parameterNames: "none",
	parameterNamesSuppressWhenArgumentMatchesName: true,
	variableTypes: false,
	variableTypesSuppressWhenTypeMatchesName: true,
};

export const CONFIG_DEFAULTS: Config = {
	cliPath: "musi",
	lspPath: "musi_lsp",
	lspEnabled: true,
	checkOnSave: true,
	hoverMaximumLength: 500,
	inlayHints: INLAY_HINTS_DEFAULTS,
	runtime: RUNTIME_DEFAULTS,
	terminal: TERMINAL_DEFAULTS,
	runConfigurations: [],
};

export function getConfig(): Config {
	const cfg = vscode.workspace.getConfiguration("musi");

	return {
		cliPath: cfg.get("cliPath", CONFIG_DEFAULTS.cliPath),
		lspPath: cfg.get("lspPath", CONFIG_DEFAULTS.lspPath),
		lspEnabled: cfg.get("lsp.enabled", CONFIG_DEFAULTS.lspEnabled),
		checkOnSave: cfg.get("checkOnSave", CONFIG_DEFAULTS.checkOnSave),
		hoverMaximumLength: cfg.get(
			"hover.maximumLength",
			CONFIG_DEFAULTS.hoverMaximumLength,
		),
		inlayHints: {
			enabled: cfg.get("inlayHints.enabled", INLAY_HINTS_DEFAULTS.enabled),
			parameterNames: cfg.get(
				"inlayHints.parameterNames.enabled",
				INLAY_HINTS_DEFAULTS.parameterNames,
			),
			parameterNamesSuppressWhenArgumentMatchesName: cfg.get(
				"inlayHints.parameterNames.suppressWhenArgumentMatchesName",
				INLAY_HINTS_DEFAULTS.parameterNamesSuppressWhenArgumentMatchesName,
			),
			variableTypes: cfg.get(
				"inlayHints.variableTypes.enabled",
				INLAY_HINTS_DEFAULTS.variableTypes,
			),
			variableTypesSuppressWhenTypeMatchesName: cfg.get(
				"inlayHints.variableTypes.suppressWhenTypeMatchesName",
				INLAY_HINTS_DEFAULTS.variableTypesSuppressWhenTypeMatchesName,
			),
		},
		runtime: {
			args: cfg.get("runtime.args", RUNTIME_DEFAULTS.args),
			env: cfg.get("runtime.env", RUNTIME_DEFAULTS.env),
			envFile: cfg.get("runtime.envFile", RUNTIME_DEFAULTS.envFile),
			cwd: cfg.get("runtime.cwd", RUNTIME_DEFAULTS.cwd),
		},
		terminal: {
			clearBeforeRun: cfg.get(
				"terminal.clearBeforeRun",
				TERMINAL_DEFAULTS.clearBeforeRun,
			),
			focusOnRun: cfg.get("terminal.focusOnRun", TERMINAL_DEFAULTS.focusOnRun),
			reuseTerminal: cfg.get(
				"terminal.reuseTerminal",
				TERMINAL_DEFAULTS.reuseTerminal,
			),
		},
		runConfigurations: cfg.get(
			"runConfigurations",
			CONFIG_DEFAULTS.runConfigurations,
		),
	};
}

export function onConfigChange(
	callback: (event: vscode.ConfigurationChangeEvent) => void,
): vscode.Disposable {
	return vscode.workspace.onDidChangeConfiguration((event) => {
		if (event.affectsConfiguration("musi")) {
			callback(event);
		}
	});
}
