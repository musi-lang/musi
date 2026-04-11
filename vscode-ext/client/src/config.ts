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
	readonly checkOnSave: boolean;
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

export const CONFIG_DEFAULTS: Config = {
	cliPath: "musi",
	lspPath: "musi_lsp",
	checkOnSave: true,
	runtime: RUNTIME_DEFAULTS,
	terminal: TERMINAL_DEFAULTS,
	runConfigurations: [],
};

export function getConfig(): Config {
	const cfg = vscode.workspace.getConfiguration("musi");

	return {
		cliPath: cfg.get("cliPath", CONFIG_DEFAULTS.cliPath),
		lspPath: cfg.get("lspPath", CONFIG_DEFAULTS.lspPath),
		checkOnSave: cfg.get("checkOnSave", CONFIG_DEFAULTS.checkOnSave),
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
