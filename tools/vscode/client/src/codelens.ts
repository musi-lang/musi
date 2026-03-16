import * as path from "node:path";
import { parse as parseTOML } from "smol-toml";
import * as vscode from "vscode";
import type { MsPackage } from "./types.ts";

/**
 * Provides CodeLens "▶ <task>" buttons above each task entry in mspackage.toml.
 */
export class MsPackageCodeLensProvider implements vscode.CodeLensProvider {
	provideCodeLenses(document: vscode.TextDocument): vscode.CodeLens[] {
		let pkg: MsPackage;
		try {
			pkg = parseTOML(document.getText()) as typeof pkg;
		} catch {
			return [];
		}

		if (!pkg.tasks) {
			return [];
		}

		const pkgDir = path.dirname(document.uri.fsPath);
		const lenses: vscode.CodeLens[] = [];

		for (const [name, entry] of Object.entries(pkg.tasks)) {
			const cmd = typeof entry === "string" ? entry : entry.command;
			const escaped = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
			const tableRegex = new RegExp(`^\\[tasks\\.${escaped}\\]`);
			const inlineRegex = new RegExp(`^${escaped}\\s*=`);

			for (let i = 0; i < document.lineCount; i++) {
				const line = document.lineAt(i).text;
				if (tableRegex.test(line) || inlineRegex.test(line)) {
					lenses.push(
						new vscode.CodeLens(new vscode.Range(i, 0, i, 0), {
							title: `▶ ${name}`,
							command: "musi.runTaskCommand",
							arguments: [cmd, pkgDir],
						}),
					);
					break;
				}
			}
		}

		return lenses;
	}
}

export class MsTestCodeLensProvider implements vscode.CodeLensProvider {
	provideCodeLenses(document: vscode.TextDocument): vscode.CodeLens[] {
		if (!document.uri.fsPath.endsWith(".test.ms")) {
			return [];
		}

		const range = new vscode.Range(0, 0, 0, 0);
		return [
			new vscode.CodeLens(range, {
				title: "$(play) Run Test",
				command: "musi.runTest",
				arguments: [document.uri.fsPath],
			}),
			new vscode.CodeLens(range, {
				title: "$(bug) Debug",
				command: "musi.debugTest",
				arguments: [document.uri.fsPath],
			}),
		];
	}
}
