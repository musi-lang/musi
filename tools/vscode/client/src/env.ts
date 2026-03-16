import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";

export function parseEnvFile(filePath: string): Record<string, string> {
	const env: Record<string, string> = {};
	if (!(filePath && fs.existsSync(filePath))) {
		return env;
	}

	const content = fs.readFileSync(filePath, "utf8");
	for (const line of content.split("\n")) {
		const trimmed = line.trim();
		if (!trimmed || trimmed.startsWith("#")) {
			continue;
		}
		const eqIdx = trimmed.indexOf("=");
		if (eqIdx < 0) {
			continue;
		}
		const key = trimmed.slice(0, eqIdx).trim();
		let value = trimmed.slice(eqIdx + 1).trim();
		if (
			(value.startsWith('"') && value.endsWith('"')) ||
			(value.startsWith("'") && value.endsWith("'"))
		) {
			value = value.slice(1, -1);
		}
		env[key] = value;
	}
	return env;
}

export function resolveEnvFile(envFile: string): string {
	if (!envFile) {
		return "";
	}
	if (path.isAbsolute(envFile)) {
		return envFile;
	}
	const wsFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
	if (!wsFolder) {
		return envFile;
	}
	return path.join(wsFolder, envFile);
}

export function mergeEnv(
	...sources: Record<string, string>[]
): Record<string, string> {
	const merged: Record<string, string> = {};
	for (const src of sources) {
		Object.assign(merged, src);
	}
	return merged;
}
