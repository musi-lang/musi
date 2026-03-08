import * as path from "node:path";
import * as vscode from "vscode";

interface MsPackageTask {
	command: string;
	description?: string;
}

/**
 * Provides CodeLens "▶ <task>" buttons above each task entry in mspackage.json.
 */
export class MsPackageCodeLensProvider implements vscode.CodeLensProvider {
	provideCodeLenses(document: vscode.TextDocument): vscode.CodeLens[] {
		let pkg: { tasks?: Record<string, string | MsPackageTask> };
		try {
			pkg = JSON.parse(document.getText()) as typeof pkg;
		} catch {
			return [];
		}

		if (!pkg.tasks) return [];

		const pkgDir = path.dirname(document.uri.fsPath);
		const lenses: vscode.CodeLens[] = [];

		for (const [name, entry] of Object.entries(pkg.tasks)) {
			const cmd =
				typeof entry === "string" ? entry : entry.command;
			const keyRegex = new RegExp(`"${name}"\\s*:`);

			for (let i = 0; i < document.lineCount; i++) {
				if (keyRegex.test(document.lineAt(i).text)) {
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
