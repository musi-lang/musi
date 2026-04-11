import * as vscode from "vscode";
import { taskNameLineMap } from "./manifest.ts";

export class MsPackageCodeLensProvider implements vscode.CodeLensProvider {
	provideCodeLenses(document: vscode.TextDocument): vscode.CodeLens[] {
		const lines = taskNameLineMap(document);
		const lenses: vscode.CodeLens[] = [];

		for (const [taskName, line] of lines) {
			lenses.push(
				new vscode.CodeLens(new vscode.Range(line, 0, line, 0), {
					title: `▶ ${taskName}`,
					command: "musi.runTaskByName",
					arguments: [document.uri.toString(), taskName],
				}),
			);
		}

		return lenses;
	}
}
