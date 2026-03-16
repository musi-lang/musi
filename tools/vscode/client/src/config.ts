import * as vscode from "vscode";

export interface RuntimeConfig {
	readonly args: string[];
	readonly env: Record<string, string>;
	readonly inheritEnv: boolean;
	readonly envFile: string;
	readonly cwd: string;
}

export interface CompilerConfig {
	readonly path: string | null;
	readonly args: string[];
	readonly target: string;
	readonly strict: boolean;
	readonly noEmitOnError: boolean;
	readonly buildBeforeRun: boolean;
}

export interface TerminalConfig {
	readonly clearBeforeRun: boolean;
	readonly focusOnRun: boolean;
	readonly reuseTerminal: boolean;
}

export interface RunConfiguration {
	readonly name: string;
	readonly file?: string;
	readonly compilerArgs?: string[];
	readonly runtimeArgs?: string[];
	readonly env?: Record<string, string>;
	readonly envFile?: string;
	readonly cwd?: string;
	readonly preLaunchTask?: string;
}

/**
 * Extension configuration values from VS Code settings.
 */
export interface Config {
	readonly lspPath: string;
	readonly cliPath: string;
	readonly checkOnSave: boolean;
	readonly traceServer: "off" | "messages" | "verbose";
	readonly runtime: RuntimeConfig;
	readonly compiler: CompilerConfig;
	readonly terminal: TerminalConfig;
	readonly runConfigurations: RunConfiguration[];
}

const RUNTIME_DEFAULTS: RuntimeConfig = {
	args: [],
	env: {},
	inheritEnv: true,
	envFile: "",
	cwd: "",
};

const COMPILER_DEFAULTS: CompilerConfig = {
	path: null,
	args: [],
	target: "MS2025",
	strict: false,
	noEmitOnError: true,
	buildBeforeRun: true,
};

const TERMINAL_DEFAULTS: TerminalConfig = {
	clearBeforeRun: false,
	focusOnRun: true,
	reuseTerminal: true,
};

export const CONFIG_DEFAULTS: Config = {
	lspPath: "music_lsp",
	cliPath: "musi",
	checkOnSave: true,
	traceServer: "off",
	runtime: RUNTIME_DEFAULTS,
	compiler: COMPILER_DEFAULTS,
	terminal: TERMINAL_DEFAULTS,
	runConfigurations: [],
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
		traceServer: cfg.get("trace.server", CONFIG_DEFAULTS.traceServer),
		runtime: {
			args: cfg.get("runtime.args", RUNTIME_DEFAULTS.args),
			env: cfg.get("runtime.env", RUNTIME_DEFAULTS.env),
			inheritEnv: cfg.get("runtime.inheritEnv", RUNTIME_DEFAULTS.inheritEnv),
			envFile: cfg.get("runtime.envFile", RUNTIME_DEFAULTS.envFile),
			cwd: cfg.get("runtime.cwd", RUNTIME_DEFAULTS.cwd),
		},
		compiler: {
			path: cfg.get("compiler.path", COMPILER_DEFAULTS.path),
			args: cfg.get("compiler.args", COMPILER_DEFAULTS.args),
			target: cfg.get("compiler.target", COMPILER_DEFAULTS.target),
			strict: cfg.get("compiler.strict", COMPILER_DEFAULTS.strict),
			noEmitOnError: cfg.get(
				"compiler.noEmitOnError",
				COMPILER_DEFAULTS.noEmitOnError,
			),
			buildBeforeRun: cfg.get(
				"compiler.buildBeforeRun",
				COMPILER_DEFAULTS.buildBeforeRun,
			),
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
