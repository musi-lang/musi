import * as vscode from "vscode";
import { taskNameLineMap } from "./manifest/manifest.ts";

export class MsPackageCodeLensProvider {
	provideCodeLenses(
		document: vscode.TextDocument,
		_token: vscode.CancellationToken,
	): vscode.CodeLens[] {
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
