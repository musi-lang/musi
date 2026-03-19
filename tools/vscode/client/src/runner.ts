import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import { findCliPath, showCliNotFoundUI } from "./bootstrap.ts";
import type { RunConfiguration } from "./config.ts";
import { getConfig } from "./config.ts";
import { mergeEnv, parseEnvFile, resolveEnvFile } from "./env.ts";
import { TERMINAL_NAME } from "./utils.ts";

let _cachedCompilerPath: string | undefined;

export interface ExecutionRequest {
	readonly file: string;
	readonly compilerArgs: string[];
	readonly runtimeArgs: string[];
	readonly env: Record<string, string>;
	readonly cwd: string;
	readonly buildBeforeRun: boolean;
}

export function buildExecutionRequest(
	file: string,
	runConfig?: RunConfiguration,
): ExecutionRequest {
	const config = getConfig();
	const rt = config.runtime;
	const cc = config.compiler;

	const envFileVars = parseEnvFile(
		resolveEnvFile(runConfig?.envFile ?? rt.envFile),
	);
	const env = mergeEnv(envFileVars, rt.env, runConfig?.env ?? {});

	return {
		file: runConfig?.file ?? file,
		compilerArgs: [...cc.args, ...(runConfig?.compilerArgs ?? [])],
		runtimeArgs: [...rt.args, ...(runConfig?.runtimeArgs ?? [])],
		env,
		cwd: runConfig?.cwd ?? rt.cwd,
		buildBeforeRun: cc.buildBeforeRun,
	};
}

export async function findCompilerPath(): Promise<string | undefined> {
	if (_cachedCompilerPath) {
		return _cachedCompilerPath;
	}

	const config = getConfig();
	if (config.compiler.path) {
		if (fs.existsSync(config.compiler.path)) {
			_cachedCompilerPath = config.compiler.path;
			return _cachedCompilerPath;
		}
	}

	const cliPath = findCliPath();
	if (cliPath) {
		// musi compiler is co-located with the musi CLI - both built from the same cargo workspace
		const dir = path.dirname(cliPath);
		const musicPath = path.join(dir, "musi");
		if (fs.existsSync(musicPath)) {
			_cachedCompilerPath = musicPath;
			return _cachedCompilerPath;
		}
	}

	return undefined;
}

export function clearCompilerPathCache() {
	_cachedCompilerPath = undefined;
}

export async function executeInTerminal(
	request: ExecutionRequest,
	subcommand: "run" | "build" | "check" | "test",
	nameFilter?: string,
): Promise<void> {
	const config = getConfig();
	const tc = config.terminal;

	const compilerPath = await findCompilerPath();
	if (!compilerPath) {
		await showCliNotFoundUI();
		return;
	}

	const args: string[] = [subcommand];
	if (subcommand === "run" || subcommand === "build") {
		args.push(...request.compilerArgs);
	}
	args.push(JSON.stringify(request.file));
	if (subcommand === "test" && nameFilter) {
		args.push("--name", JSON.stringify(nameFilter));
	}
	if (subcommand === "run" && request.runtimeArgs.length > 0) {
		args.push("--", ...request.runtimeArgs);
	}

	const cmd = `${JSON.stringify(compilerPath)} ${args.join(" ")}`;

	const terminalOptions: vscode.TerminalOptions = {
		name: TERMINAL_NAME,
		...(Object.keys(request.env).length > 0 ? { env: request.env } : {}),
		...(request.cwd ? { cwd: request.cwd } : {}),
	};

	let terminal: vscode.Terminal;
	if (tc.reuseTerminal) {
		terminal =
			vscode.window.terminals.find((t) => t.name === TERMINAL_NAME) ??
			vscode.window.createTerminal(terminalOptions);
	} else {
		terminal = vscode.window.createTerminal(terminalOptions);
	}

	if (tc.clearBeforeRun) {
		terminal.sendText("clear");
	}
	if (tc.focusOnRun) {
		terminal.show();
	}
	terminal.sendText(cmd);
}
