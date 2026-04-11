import { spawn } from "node:child_process";
import * as path from "node:path";
import * as vscode from "vscode";
import { findCliPath, showCliNotFoundUI } from "./bootstrap.ts";
import type { RunConfiguration } from "./config.ts";
import { getConfig } from "./config.ts";
import { mergeEnv, parseEnvFile, resolveEnvFile } from "./env.ts";
import type { MsTaskSpec, PackageRoot } from "./types.ts";
import { TERMINAL_NAME } from "./utils.ts";

export interface PackageExecutionRequest {
	readonly pkg: PackageRoot;
	readonly entry?: string;
	readonly cliArgs: string[];
	readonly runtimeArgs: string[];
	readonly env: Record<string, string>;
	readonly cwd: string;
	readonly preLaunchTask?: string;
}

export interface DiagnosticRangePoint {
	readonly line: number;
	readonly character: number;
}

export interface FlatDiagnosticRangePayload {
	readonly start_line: number;
	readonly start_col: number;
	readonly end_line: number;
	readonly end_col: number;
}

export interface DiagnosticRangePayload {
	readonly start: DiagnosticRangePoint;
	readonly end: DiagnosticRangePoint;
}

type RawDiagnosticRangePayload =
	| DiagnosticRangePayload
	| FlatDiagnosticRangePayload;

export interface DiagnosticLabelPayload {
	readonly file?: string;
	readonly message?: string;
	readonly range?: DiagnosticRangePayload;
}

export interface DiagnosticPayload {
	readonly file?: string;
	readonly severity?: string;
	readonly level?: string;
	readonly code?: string;
	readonly message: string;
	readonly range?: DiagnosticRangePayload;
	readonly primaryRange?: DiagnosticRangePayload;
	readonly labels?: DiagnosticLabelPayload[];
	readonly notes?: string[];
	readonly hint?: string;
}

export interface StructuredDiagnosticsPayload {
	readonly diagnostics: DiagnosticPayload[];
}

export interface StructuredCheckResult {
	readonly exitCode: number;
	readonly stdout: string;
	readonly stderr: string;
	readonly payload: StructuredDiagnosticsPayload;
}

function normalizeRangePoint(value: unknown): DiagnosticRangePoint | undefined {
	if (!(value && typeof value === "object")) {
		return undefined;
	}
	const point = value as Partial<DiagnosticRangePoint>;
	if (typeof point.line !== "number" || typeof point.character !== "number") {
		return undefined;
	}
	return { line: point.line, character: point.character };
}

function normalizeRange(value: unknown): DiagnosticRangePayload | undefined {
	if (!(value && typeof value === "object")) {
		return undefined;
	}
	const range = value as Partial<DiagnosticRangePayload> &
		Partial<FlatDiagnosticRangePayload>;
	const start = normalizeRangePoint(range.start);
	const end = normalizeRangePoint(range.end);
	if (start && end) {
		return { start, end };
	}
	if (
		typeof range.start_line === "number" &&
		typeof range.start_col === "number" &&
		typeof range.end_line === "number" &&
		typeof range.end_col === "number"
	) {
		return {
			start: { line: range.start_line, character: range.start_col },
			end: { line: range.end_line, character: range.end_col },
		};
	}
	return undefined;
}

function normalizeLabel(value: unknown): DiagnosticLabelPayload | undefined {
	if (!(value && typeof value === "object")) {
		return undefined;
	}
	const label = value as {
		file?: unknown;
		message?: unknown;
		range?: RawDiagnosticRangePayload;
	};
	const range =
		label.range !== undefined ? normalizeRange(label.range) : undefined;
	return {
		...(typeof label.file === "string" ? { file: label.file } : {}),
		...(typeof label.message === "string" ? { message: label.message } : {}),
		...(range ? { range } : {}),
	};
}

function normalizeDiagnostic(value: unknown): DiagnosticPayload | undefined {
	if (!(value && typeof value === "object")) {
		return undefined;
	}
	const payload = value as {
		file?: unknown;
		severity?: unknown;
		level?: unknown;
		code?: unknown;
		message?: unknown;
		range?: RawDiagnosticRangePayload;
		primaryRange?: RawDiagnosticRangePayload;
		labels?: unknown;
		notes?: unknown;
		hint?: unknown;
	};
	if (typeof payload.message !== "string") {
		return undefined;
	}
	const range =
		payload.range !== undefined ? normalizeRange(payload.range) : undefined;
	const primaryRange =
		payload.primaryRange !== undefined
			? normalizeRange(payload.primaryRange)
			: undefined;
	return {
		message: payload.message,
		...(typeof payload.file === "string" ? { file: payload.file } : {}),
		...(typeof payload.severity === "string"
			? { severity: payload.severity }
			: {}),
		...(typeof payload.level === "string" ? { level: payload.level } : {}),
		...(typeof payload.code === "string" ? { code: payload.code } : {}),
		...(range ? { range } : {}),
		...(primaryRange ? { primaryRange } : {}),
		...(Array.isArray(payload.labels)
			? {
					labels: payload.labels
						.map(normalizeLabel)
						.filter(
							(entry): entry is DiagnosticLabelPayload => entry !== undefined,
						),
				}
			: {}),
		...(Array.isArray(payload.notes)
			? {
					notes: payload.notes.filter(
						(entry): entry is string => typeof entry === "string",
					),
				}
			: {}),
		...(typeof payload.hint === "string" ? { hint: payload.hint } : {}),
	};
}

function shellJoin(parts: readonly string[]): string {
	return parts.map((part) => JSON.stringify(part)).join(" ");
}

function resolveCwd(pkg: PackageRoot, cwdOverride?: string): string {
	if (!cwdOverride) {
		return pkg.rootDir;
	}
	if (path.isAbsolute(cwdOverride)) {
		return cwdOverride;
	}
	return path.join(pkg.rootDir, cwdOverride);
}

function buildEnv(
	pkg: PackageRoot,
	runConfig?: RunConfiguration,
): Record<string, string> {
	const runtime = getConfig().runtime;
	const envFileVars = parseEnvFile(
		resolveEnvFile(runConfig?.envFile ?? runtime.envFile, pkg.rootDir),
	);
	return mergeEnv(runtime.env, envFileVars, runConfig?.env ?? {});
}

function terminalForRequest(request: PackageExecutionRequest): vscode.Terminal {
	const terminalConfig = getConfig().terminal;
	const options: vscode.TerminalOptions = {
		name: TERMINAL_NAME,
		cwd: request.cwd,
		...(Object.keys(request.env).length > 0 ? { env: request.env } : {}),
	};

	if (terminalConfig.reuseTerminal) {
		return (
			vscode.window.terminals.find(
				(terminal) => terminal.name === TERMINAL_NAME,
			) ?? vscode.window.createTerminal(options)
		);
	}
	return vscode.window.createTerminal(options);
}

export function buildPackageExecutionRequest(
	pkg: PackageRoot,
	runConfig?: RunConfiguration,
): PackageExecutionRequest {
	const config = getConfig();
	return {
		pkg,
		...(runConfig?.entry !== undefined ? { entry: runConfig.entry } : {}),
		cliArgs: [...(runConfig?.cliArgs ?? [])],
		runtimeArgs: [...config.runtime.args, ...(runConfig?.runtimeArgs ?? [])],
		env: buildEnv(pkg, runConfig),
		cwd: resolveCwd(pkg, runConfig?.cwd ?? config.runtime.cwd),
		...(runConfig?.preLaunchTask !== undefined
			? { preLaunchTask: runConfig.preLaunchTask }
			: {}),
	};
}

export async function executePackageCommandInTerminal(
	request: PackageExecutionRequest,
	subcommand: "run" | "build" | "test",
	taskPlan: readonly MsTaskSpec[] = [],
): Promise<void> {
	const cliPath = findCliPath();
	if (!cliPath) {
		await showCliNotFoundUI();
		return;
	}

	const terminal = terminalForRequest(request);
	const terminalConfig = getConfig().terminal;
	const commands = taskPlan.map((task) => task.command);
	const args = [cliPath, subcommand, ...request.cliArgs];

	if (request.entry) {
		args.push(request.entry);
	}
	if (subcommand === "run" && request.runtimeArgs.length > 0) {
		args.push("--", ...request.runtimeArgs);
	}

	commands.push(shellJoin(args));

	if (terminalConfig.clearBeforeRun) {
		terminal.sendText("clear");
	}
	if (terminalConfig.focusOnRun) {
		terminal.show();
	}
	terminal.sendText(
		`cd ${JSON.stringify(request.cwd)} && ${commands.join(" && ")}`,
	);
}

export async function executeTaskPlanInTerminal(
	request: PackageExecutionRequest,
	taskPlan: readonly MsTaskSpec[],
): Promise<void> {
	const terminal = terminalForRequest(request);
	const terminalConfig = getConfig().terminal;
	const commands = taskPlan.map((task) => task.command);
	if (commands.length === 0) {
		return;
	}

	if (terminalConfig.clearBeforeRun) {
		terminal.sendText("clear");
	}
	if (terminalConfig.focusOnRun) {
		terminal.show();
	}
	terminal.sendText(
		`cd ${JSON.stringify(request.cwd)} && ${commands.join(" && ")}`,
	);
}

function parseStructuredDiagnostics(
	stdout: string,
	stderr: string,
): StructuredDiagnosticsPayload {
	const parseCandidate = (
		text: string,
	): StructuredDiagnosticsPayload | undefined => {
		const trimmed = text.trim();
		if (!trimmed) {
			return undefined;
		}

		const parsed = JSON.parse(trimmed) as
			| StructuredDiagnosticsPayload
			| DiagnosticPayload[];
		if (Array.isArray(parsed)) {
			return {
				diagnostics: parsed
					.map(normalizeDiagnostic)
					.filter((entry): entry is DiagnosticPayload => entry !== undefined),
			};
		}
		if (Array.isArray(parsed.diagnostics)) {
			return {
				diagnostics: parsed.diagnostics
					.map(normalizeDiagnostic)
					.filter((entry): entry is DiagnosticPayload => entry !== undefined),
			};
		}
		throw new Error("structured diagnostics payload missing `diagnostics`");
	};

	return (
		parseCandidate(stdout) ?? parseCandidate(stderr) ?? { diagnostics: [] }
	);
}

export async function runStructuredPackageCheck(
	pkg: PackageRoot,
	signal?: AbortSignal,
): Promise<StructuredCheckResult> {
	const cliPath = findCliPath();
	if (!cliPath) {
		throw new Error("Musi CLI binary not found");
	}

	const request = buildPackageExecutionRequest(pkg);
	const args = ["check", "--diagnostics-format", "json"];

	return new Promise((resolve, reject) => {
		const proc = spawn(cliPath, args, {
			cwd: request.cwd,
			env: { ...process.env, ...request.env },
			stdio: ["ignore", "pipe", "pipe"],
			signal,
		});

		let stdout = "";
		let stderr = "";

		proc.stdout.on("data", (chunk: Buffer) => {
			stdout += chunk.toString();
		});
		proc.stderr.on("data", (chunk: Buffer) => {
			stderr += chunk.toString();
		});
		proc.on("error", (error) => {
			reject(error);
		});
		proc.on("close", (code) => {
			try {
				resolve({
					exitCode: code ?? 1,
					stdout,
					stderr,
					payload: parseStructuredDiagnostics(stdout, stderr),
				});
			} catch (error) {
				reject(error);
			}
		});
	});
}
