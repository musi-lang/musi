import * as path from "node:path";
import * as vscode from "vscode";
import type { MsPackage } from "./types.ts";

/**
 * Provides CodeLens "▶ <task>" buttons above each task entry in musi.json.
 */
export class MsPackageCodeLensProvider implements vscode.CodeLensProvider {
	provideCodeLenses(document: vscode.TextDocument): vscode.CodeLens[] {
		let pkg: MsPackage;
		try {
			pkg = JSON.parse(document.getText()) as typeof pkg;
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
			const keyRegex = new RegExp(`"${escaped}"\\s*:`);

			for (let i = 0; i < document.lineCount; i++) {
				const line = document.lineAt(i).text;
				if (keyRegex.test(line)) {
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
